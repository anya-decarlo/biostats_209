from flask import Flask, request, Response, redirect
import datetime

app = Flask(__name__)

ADMIN_PASSWORD = "yourSecretPassword"  # change to something strong and private

@app.route("/")
def index():
    return redirect("/click")

@app.route("/click")
def track_click():
    user = request.args.get('user', 'unknown')
    ip = request.remote_addr or request.environ.get('HTTP_X_FORWARDED_FOR', 'unknown')
    user_agent = request.headers.get('User-Agent', 'unknown')
    referrer = request.headers.get('Referer', 'none')
    time = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')

    log_entry = (
        f"[{time}] — Click Event\n"
        f"User tag: {user}\n"
        f"IP Address: {ip}\n"
        f"User-Agent: {user_agent}\n"
        f"Referrer: {referrer}\n"
        f"{'-'*40}"
    )

    with open("click_log.txt", "a") as f:
        f.write(log_entry + "\n")

    return '''
    <html>
        <body>
            <script>
                window.open("https://soundcloud.com/lala-789978498/song1-4_15_25-1-41-pm", "_blank");
            </script>
            <h3>Click logged successfully! ✅</h3>
            <p>If the link didn't open automatically, <a href="https://soundcloud.com/lala-789978498/song1-4_15_25-1-41-pm" target="_blank">click here</a></p>
        </body>
    </html>
    '''

@app.route("/view-logs")
def view_logs():
    password = request.args.get('pw', '')
    if password != ADMIN_PASSWORD:
        return Response("Unauthorized", status=401)

    try:
        with open("click_log.txt", "r") as f:
            content = f.read()
        return f"<pre>{content}</pre>"
    except FileNotFoundError:
        return "No logs yet."

app.run(host='0.0.0.0', port=81)