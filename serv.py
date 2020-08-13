import os
from flask import Flask, flash, request, redirect, url_for

app = Flask(__name__)
app.config['UPLOAD_FOLDER'] = '/Users/dest/pollen_workshop'

@app.route('/')
def root():
    return open('index.html').read()

@app.route('/editor')
def editor():
    return open('editor.html').read()

@app.route('/upload', methods=['PUT'])
def upload_file():
    fn = request.args.get('filename')
    with open(os.path.join(app.config['UPLOAD_FOLDER'], fn), 'wb') as f:
        f.write(request.data)
    return 'file loaded!'

if __name__ == '__main__':
    app.secret_key = 'super secret key'
    app.config['SESSION_TYPE'] = 'filesystem'

    app.run(host='0.0.0.0', port=5000)
