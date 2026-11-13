#!/usr/bin/env python3
"""A fake Jira Cloud on this machine, for trying shipit without a real site.

It answers the part of Jira's REST API that shipit uses, for one
project, PRJ.  Its first issue holds a picture that is attached to it
and shown in its description.  What shipit writes is kept in memory
until the server stops.

    python3 test/fake-jira.py --port 0

prints the address it listens on as its first line.  A request must log
in with Basic auth, as a request to Jira must, but any login will do.
A request the fake does not know is answered 404 and written to
standard error, so what shipit asked for and the fake lacks shows there.

Jira reads a description or a comment written through version 2 of its
API as wiki markup, and gives it back through version 3 as its own
document format.  The fake does the same for plain lines, for
`!file.png!`, which embeds an attachment of the issue as an image, and
for `{code:LANGUAGE}...{code}`, which is a block of code.

The move to Done needs a resolution, as in a common workflow of Jira,
and the other moves take none.  As in Jira, the moves list their fields
only when the request asks for `expand=transitions.fields`, and a move
that is sent a resolution it does not take is refused.
"""

import argparse
import email.parser
import email.policy
import itertools
import json
import re
import struct
import sys
import threading
import uuid
import zlib
from datetime import datetime, timezone
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlsplit

USER = {
    "accountId": "sandbox-account-1",
    "displayName": "Sandbox User",
    "emailAddress": "sandbox@example.com",
    "active": True,
}

STATUSES = [
    {"id": "1", "name": "To Do", "statusCategory": {"key": "new", "name": "To Do"}},
    {"id": "3", "name": "In Progress",
     "statusCategory": {"key": "indeterminate", "name": "In Progress"}},
    {"id": "5", "name": "Done", "statusCategory": {"key": "done", "name": "Done"}},
]

RESOLUTIONS = [{"id": "1", "name": "Done"}, {"id": "2", "name": "Won't Do"},
               {"id": "3", "name": "Duplicate"}]

RESOLUTION_FIELD = {"required": True, "name": "Resolution", "key": "resolution",
                    "operations": ["set"], "allowedValues": RESOLUTIONS}


def fields_of_move(status):
    """The fields on the screen of the move to STATUS, by their keys."""
    return {"resolution": RESOLUTION_FIELD} if status["name"] == "Done" else {}


TRANSITIONS = [{"id": str(10 + int(status["id"])), "name": status["name"], "to": status,
                "fields": fields_of_move(status)}
               for status in STATUSES]

ISSUE_TYPES = [{"id": "10001", "name": "Bug"}, {"id": "10002", "name": "Task"}]

EMBED = re.compile(r"!([^!|\n]+)(?:\|[^!\n]*)?!")
# {code:python}...{code}, whose lines Jira keeps as they are written,
# blank ones too.
CODE = re.compile(r"\{code(?::([^}|]*))?(?:\|[^}]*)?\}\n?(.*?)\n?\{code\}", re.S)


def now():
    """The time now, as Jira writes a time."""
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.000+0000")


def png(width=16, height=16, rgb=(203, 36, 49)):
    """A PNG of one colour, made here so the fake needs no file."""
    row = b"\x00" + bytes(rgb) * width
    def chunk(kind, data):
        return (struct.pack(">I", len(data)) + kind + data
                + struct.pack(">I", zlib.crc32(kind + data) & 0xFFFFFFFF))
    return (b"\x89PNG\r\n\x1a\n"
            + chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0))
            + chunk(b"IDAT", zlib.compress(row * height))
            + chunk(b"IEND", b""))


def paragraph(text):
    """An ADF paragraph of TEXT, its line breaks kept."""
    content = []
    for number, line in enumerate(text.split("\n")):
        if number:
            content.append({"type": "hardBreak"})
        if line:
            content.append({"type": "text", "text": line})
    return {"type": "paragraph", "content": content}


def media(filename):
    """An ADF block that shows the attachment called FILENAME."""
    return {"type": "mediaSingle", "attrs": {"layout": "align-start"},
            "content": [{"type": "media",
                         "attrs": {"type": "file", "collection": "", "alt": filename,
                                   "id": str(uuid.uuid5(uuid.NAMESPACE_URL, filename))}}]}


def code_block(language, code):
    """An ADF code block of CODE, in LANGUAGE when one is named."""
    block = {"type": "codeBlock", "content": [{"type": "text", "text": code}]}
    if language:
        block["attrs"] = {"language": language}
    return block


def prose_to_adf(text):
    """The ADF blocks of TEXT, wiki markup that holds no code block."""
    blocks = []
    for block in re.split(r"\n\s*\n", text.strip()):
        start = 0
        for embed in EMBED.finditer(block):
            before = block[start:embed.start()].strip("\n")
            if before:
                blocks.append(paragraph(before))
            blocks.append(media(embed.group(1)))
            start = embed.end()
        rest = block[start:].strip("\n")
        if rest:
            blocks.append(paragraph(rest))
    return blocks


def to_adf(text):
    """TEXT, wiki markup as version 2 takes it, in Jira's document format."""
    text = text or ""
    blocks = []
    start = 0
    for code in CODE.finditer(text):
        blocks.extend(prose_to_adf(text[start:code.start()]))
        blocks.append(code_block(code.group(1), code.group(2)))
        start = code.end()
    blocks.extend(prose_to_adf(text[start:]))
    return {"type": "doc", "version": 1, "content": blocks}


class Store:
    """The issues, comments and attachments of the fake, in memory."""

    def __init__(self):
        self.lock = threading.Lock()
        self.ids = itertools.count(10000)
        self.numbers = {}
        self.issues = {}
        self.attachments = {}
        first = self.new_issue("PRJ", "A bug to try shipit on", "Bug",
                               "An issue of the sandbox, with a picture attached.\n\n!fake.png!")
        self.attach(first["key"], "fake.png", "image/png", png())
        self.new_issue("PRJ", "A task with nothing attached", "Task", "Plain text.")

    def new_issue(self, project, summary, issue_type, description):
        """Make an issue of PROJECT, and return it."""
        number = self.numbers.get(project, 0) + 1
        self.numbers[project] = number
        stamp = now()
        issue = {"id": str(next(self.ids)), "key": f"{project}-{number}", "project": project,
                 "summary": summary, "type": issue_type, "description": description,
                 "status": STATUSES[0], "resolution": None, "priority": "Medium",
                 "created": stamp, "updated": stamp, "comments": [], "attachments": []}
        self.issues[issue["key"]] = issue
        return issue

    def attach(self, key, filename, mime_type, data):
        """Attach DATA to issue KEY as FILENAME, and return the attachment."""
        attachment = {"id": str(next(self.ids)), "filename": filename, "mimeType": mime_type,
                      "data": data, "created": now()}
        self.attachments[attachment["id"]] = attachment
        self.issues[key]["attachments"].append(attachment)
        return attachment

    def add_comment(self, key, body):
        """Add a comment saying BODY to issue KEY, and return it."""
        stamp = now()
        comment = {"id": str(next(self.ids)), "body": body, "created": stamp, "updated": stamp}
        self.issues[key]["comments"].append(comment)
        self.issues[key]["updated"] = stamp
        return comment


def attachment_json(attachment, base):
    """ATTACHMENT as Jira gives it, its content at BASE."""
    return {"id": attachment["id"], "filename": attachment["filename"],
            "mimeType": attachment["mimeType"], "size": len(attachment["data"]),
            "created": attachment["created"], "author": USER,
            "content": f"{base}/rest/api/3/attachment/content/{attachment['id']}"}


def comment_json(comment, key, base, version):
    """COMMENT of issue KEY as API VERSION gives it: wiki text for 2, ADF for 3."""
    return {"id": comment["id"],
            "self": f"{base}/rest/api/{version}/issue/{key}/comment/{comment['id']}",
            "author": USER, "updateAuthor": USER,
            "body": comment["body"] if version == 2 else to_adf(comment["body"]),
            "created": comment["created"], "updated": comment["updated"]}


def issue_json(issue, base):
    """ISSUE as version 3 of the API gives it."""
    comments = [comment_json(comment, issue["key"], base, 3) for comment in issue["comments"]]
    return {"id": issue["id"], "key": issue["key"],
            "self": f"{base}/rest/api/3/issue/{issue['id']}",
            "fields": {
                "summary": issue["summary"],
                "description": to_adf(issue["description"]),
                "status": issue["status"],
                "resolution": issue["resolution"] and {"name": issue["resolution"]},
                "issuetype": next(t for t in ISSUE_TYPES if t["name"] == issue["type"]),
                "priority": {"name": issue["priority"]},
                "project": {"key": issue["project"], "name": "Sandbox"},
                "reporter": USER, "creator": USER, "assignee": USER,
                "created": issue["created"], "updated": issue["updated"],
                "labels": [], "components": [], "issuelinks": [],
                "attachment": [attachment_json(a, base) for a in issue["attachments"]],
                "comment": {"comments": comments, "total": len(comments),
                            "maxResults": len(comments), "startAt": 0}},
            "changelog": {"startAt": 0, "maxResults": 0, "total": 0, "histories": []}}


def matching_issues(store, jql):
    """The issues JQL asks for: by key, by project, or else all of them."""
    keys = re.findall(r"\b([A-Z][A-Z0-9]*-\d+)\b", jql)
    projects = re.findall(r"project\s*(?:=|in)\s*\(?\s*\"?([A-Z][A-Z0-9]*)", jql)
    return [issue for issue in store.issues.values()
            if (not keys or issue["key"] in keys)
            and (not projects or issue["project"] in projects)]


class Handler(BaseHTTPRequestHandler):
    """Answer one request, from the routes below."""

    server_version = "fake-jira/1"

    def log_message(self, fmt, *args):
        if self.server.verbose:
            sys.stderr.write("fake-jira: " + (fmt % args) + "\n")

    @property
    def store(self):
        return self.server.store

    @property
    def base(self):
        return f"http://{self.headers.get('Host')}"

    def send_json(self, status, data=None):
        body = b"" if data is None else json.dumps(data).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json;charset=UTF-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def send_bytes(self, mime_type, data):
        self.send_response(200)
        self.send_header("Content-Type", mime_type)
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def refuse(self, status, message):
        self.send_json(status, {"errorMessages": [message], "errors": {}})

    def refuse_fields(self, status, errors):
        """Refuse with ERRORS, the words of Jira for each field, by its key."""
        self.send_json(status, {"errorMessages": [], "errors": errors})

    def read_body(self):
        return self.rfile.read(int(self.headers.get("Content-Length") or 0))

    def read_json(self):
        return json.loads(self.read_body() or b"{}")

    def do_GET(self):
        self.route("GET")

    def do_POST(self):
        self.route("POST")

    def do_PUT(self):
        self.route("PUT")

    def route(self, method):
        parts = urlsplit(self.path)
        query = parse_qs(parts.query)
        if method == "GET" and parts.path.startswith("/media/"):
            return self.media(parts.path.rsplit("/", 1)[-1])
        if not self.headers.get("Authorization", "").startswith("Basic "):
            return self.refuse(401, "You are not authenticated. Authentication required.")
        for route_method, pattern, answer in ROUTES:
            found = re.fullmatch(pattern, parts.path)
            if route_method == method and found:
                with self.store.lock:
                    return answer(self, *found.groups(), query=query)
        sys.stderr.write(f"fake-jira: no route for {method} {self.path}\n")
        self.refuse(404, f"fake-jira does not know {method} {parts.path}")

    def issue_or_refuse(self, key):
        issue = self.store.issues.get(key)
        if not issue:
            self.refuse(404, "Issue does not exist or you do not have permission to see it.")
        return issue

    def media(self, attachment_id):
        attachment = self.store.attachments.get(attachment_id)
        if not attachment:
            return self.refuse(404, "No such file.")
        self.send_bytes(attachment["mimeType"], attachment["data"])


def myself(h, query):
    h.send_json(200, USER)


def get_issue(h, key, query):
    issue = h.issue_or_refuse(key)
    if issue:
        h.send_json(200, issue_json(issue, h.base))


def get_comments(h, key, query):
    issue = h.issue_or_refuse(key)
    if issue:
        comments = [comment_json(c, key, h.base, 3) for c in issue["comments"]]
        h.send_json(200, {"comments": comments, "total": len(comments),
                          "maxResults": len(comments), "startAt": 0})


def add_comment(h, key, query):
    issue = h.issue_or_refuse(key)
    if issue:
        comment = h.store.add_comment(key, h.read_json().get("body", ""))
        h.send_json(201, comment_json(comment, key, h.base, 2))


def edit_comment(h, key, comment_id, query):
    issue = h.issue_or_refuse(key)
    if not issue:
        return
    comment = next((c for c in issue["comments"] if c["id"] == comment_id), None)
    if not comment:
        return h.refuse(404, "Can not find a comment for the id.")
    comment["body"] = h.read_json().get("body", "")
    comment["updated"] = now()
    h.send_json(200, comment_json(comment, key, h.base, 2))


def edit_issue(h, key, query):
    issue = h.issue_or_refuse(key)
    if not issue:
        return
    fields = h.read_json().get("fields", {})
    for field in ("summary", "description"):
        if field in fields:
            issue[field] = fields[field]
    issue["updated"] = now()
    h.send_json(204)


def create_issue(h, query):
    fields = h.read_json().get("fields", {})
    issue = h.store.new_issue(fields.get("project", {}).get("key", "PRJ"),
                              fields.get("summary", "No summary"),
                              fields.get("issuetype", {}).get("name", "Task"),
                              fields.get("description", ""))
    h.send_json(201, {"id": issue["id"], "key": issue["key"],
                      "self": f"{h.base}/rest/api/2/issue/{issue['id']}"})


def expands(query, name):
    """Whether QUERY asks to expand NAME, as expand=transitions.fields does."""
    return any(name in value.split(",") for value in query.get("expand", []))


def transition_json(transition, with_fields):
    """TRANSITION as Jira lists it, with its screen and its fields only WITH_FIELDS."""
    listed = {key: transition[key] for key in ("id", "name", "to")}
    if with_fields:
        listed["hasScreen"] = bool(transition["fields"])
        listed["fields"] = transition["fields"]
    return listed


def allowed_value(field, value):
    """The value of FIELD that VALUE, as {"id": ...} or {"name": ...}, names, or None."""
    return next((allowed for allowed in field["allowedValues"]
                 if value.get("id") == allowed["id"] or value.get("name") == allowed["name"]),
                None)


def resolution_problem(transition, value):
    """What Jira says against VALUE as the resolution of TRANSITION, or None.

    VALUE is the resolution that the request sets, or None for none.
    The words for a value that the field does not allow are the fake's
    own; the other two are those of Jira."""
    field = transition["fields"].get("resolution")
    if field is None:
        return None if value is None else ("Field 'resolution' cannot be set. It is not on"
                                           " the appropriate screen, or unknown.")
    if value is None:
        return f"{field['name']} is required." if field["required"] else None
    if allowed_value(field, value) is None:
        return f"Resolution '{value.get('name') or value.get('id')}' is not allowed here."
    return None


def resolution_after(transition, value):
    """The name of the resolution of an issue after TRANSITION, which set VALUE.

    A move out of Done clears the resolution, as the workflow of Jira does."""
    if transition["to"]["name"] != "Done":
        return None
    return allowed_value(transition["fields"]["resolution"], value)["name"]


def get_transitions(h, key, query):
    if h.issue_or_refuse(key):
        with_fields = expands(query, "transitions.fields")
        h.send_json(200, {"transitions": [transition_json(t, with_fields)
                                          for t in TRANSITIONS]})


def do_transition(h, key, query):
    issue = h.issue_or_refuse(key)
    if not issue:
        return
    body = h.read_json()
    wanted = body.get("transition", {}).get("id")
    transition = next((t for t in TRANSITIONS if t["id"] == wanted), None)
    if not transition:
        return h.refuse(400, f"Transition id '{wanted}' is not valid for this issue.")
    resolution = body.get("fields", {}).get("resolution")
    problem = resolution_problem(transition, resolution)
    if problem:
        return h.refuse_fields(400, {"resolution": problem})
    issue["status"] = transition["to"]
    issue["resolution"] = resolution_after(transition, resolution)
    issue["updated"] = now()
    h.send_json(204)


def upload(h, key, query):
    issue = h.issue_or_refuse(key)
    if not issue:
        return
    if h.headers.get("X-Atlassian-Token") != "no-check":
        return h.refuse(403, "XSRF check failed")
    form = email.parser.BytesParser(policy=email.policy.default).parsebytes(
        b"Content-Type: " + h.headers.get("Content-Type", "").encode() + b"\r\n\r\n"
        + h.read_body())
    files = [part for part in (form.iter_parts() if form.is_multipart() else [])
             if part.get_param("name", header="content-disposition") == "file"]
    if not files:
        return h.refuse(400, "No file named 'file' in the multipart body.")
    added = [h.store.attach(key, part.get_filename(), part.get_content_type(),
                            part.get_payload(decode=True))
             for part in files]
    h.send_json(200, [attachment_json(a, h.base) for a in added])


def attachment_content(h, attachment_id, query):
    attachment = h.store.attachments.get(attachment_id)
    if not attachment:
        return h.refuse(404, "The attachment does not exist.")
    if query.get("redirect") == ["false"]:
        return h.send_bytes(attachment["mimeType"], attachment["data"])
    h.send_response(303)
    h.send_header("Location", f"{h.base}/media/{attachment_id}")
    h.send_header("Content-Length", "0")
    h.end_headers()


def search(h, query):
    issues = matching_issues(h.store, query.get("jql", [""])[0])
    h.send_json(200, {"issues": [issue_json(i, h.base) for i in issues], "isLast": True})


def project(h, key, query):
    h.send_json(200, {"id": "10000", "key": key, "name": "Sandbox", "issueTypes": ISSUE_TYPES})


def project_statuses(h, key, query):
    h.send_json(200, [dict(t, statuses=STATUSES) for t in ISSUE_TYPES])


def answer_with(data):
    """A route that always answers DATA."""
    return lambda h, *groups, query: h.send_json(200, data)


ISSUE = r"([A-Z][A-Z0-9]*-\d+)"

ROUTES = [
    ("GET", r"/rest/api/3/myself", myself),
    ("GET", r"/rest/api/3/issue/" + ISSUE, get_issue),
    ("GET", r"/rest/api/3/issue/" + ISSUE + r"/comment", get_comments),
    ("POST", r"/rest/api/2/issue/" + ISSUE + r"/comment", add_comment),
    ("PUT", r"/rest/api/2/issue/" + ISSUE + r"/comment/(\d+)", edit_comment),
    ("PUT", r"/rest/api/2/issue/" + ISSUE, edit_issue),
    ("POST", r"/rest/api/2/issue", create_issue),
    ("GET", r"/rest/api/[23]/issue/" + ISSUE + r"/transitions", get_transitions),
    ("POST", r"/rest/api/[23]/issue/" + ISSUE + r"/transitions", do_transition),
    ("POST", r"/rest/api/[23]/issue/" + ISSUE + r"/attachments", upload),
    ("GET", r"/rest/api/[23]/attachment/content/(\d+)", attachment_content),
    ("GET", r"/rest/api/3/search/jql", search),
    ("GET", r"/rest/api/3/project/([A-Z][A-Z0-9]*)", project),
    ("GET", r"/rest/api/3/project/([A-Z][A-Z0-9]*)/statuses", project_statuses),
    ("GET", r"/rest/api/3/project/([A-Z][A-Z0-9]*)/components", answer_with([])),
    ("GET", r"/rest/api/2/status", answer_with(STATUSES)),
    ("GET", r"/rest/api/3/user", answer_with(USER)),
    ("GET", r"/rest/api/3/user/assignable/search", answer_with([USER])),
    ("GET", r"/rest/api/3/issue/" + ISSUE + r"/remotelink", answer_with([])),
    ("GET", r"/rest/api/3/issueLinkType", answer_with({"issueLinkTypes": []})),
    ("GET", r"/rest/api/3/label", answer_with({"values": [], "isLast": True})),
    ("POST", r"/rest/internal/2/reactions/view", answer_with([])),
]


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--port", type=int, default=0, help="0 picks a free port")
    parser.add_argument("--verbose", action="store_true", help="log every request")
    arguments = parser.parse_args()
    server = ThreadingHTTPServer(("127.0.0.1", arguments.port), Handler)
    server.store = Store()
    server.verbose = arguments.verbose
    print(f"fake-jira listening on http://127.0.0.1:{server.server_port}", flush=True)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass


if __name__ == "__main__":
    main()
