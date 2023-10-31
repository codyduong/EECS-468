/**
 * EECS 468 Assignment 6
 * Author: Cody Duong
 * Replit: https://replit.com/@CodyDuong/assignment6#index.js
 * Other Sources:
 * - https://nodejs.org/api/http.html#class-httpserver
 * - https://nodejs.org/api/http.html#requestmethod
 * - https://nodejs.org/api/fs.html#fspromisesmkdirpath-options
 * - https://nodejs.org/api/fs.html#filehandlereadfileoptions
 * - https://nodejs.org/api/fs.html#fspromisesunlinkpath
 * - https://nodejs.org/api/fs.html#filehandlecreatewritestreamoptions
 * - https://nodejs.org/api/fs.html#fspromisesmkdirpath-options
 * - https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/400
 * - https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/500
 * - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/stack
 */

const http = require('http');
const fs = require('fs');
const path = require('path');

const server = http.createServer((req, res) => {
  // try and catch so the server can still process requests even if something went wrong
  try {
    if (req.method === 'GET') {
      // GET at the URL
      const filePath = path.join(__dirname, req.url);
      fs.readFile(filePath, (err, data) => {
        // Either we found the file or we didn't
        if (err) {
          res.statusCode = 404;
          res.end('File not found');
        } else {
          res.end(data);
        }
      });
    } else if (req.method === 'PUT') {
      // PUT at the URL
      const filePath = path.join(__dirname, req.url);
      // https://stackoverflow.com/a/15630832/17954209
      if (
        req.url == '' ||
        (fs.existsSync(filePath) && fs.lstatSync(filePath).isDirectory())
      ) {
        // If empty send bad response
        res.statusCode = 400;
        res.end('Bad request');
        return;
      }
      // Pipe it in
      const writableStream = fs.createWriteStream(filePath, { flags: 'w' });
      req.pipe(writableStream);
      req.on('end', () => {
        res.end(`File ${fs.existsSync(filePath) ? 'overwritten' : 'saved'}`);
      });
    } else if (req.method === 'DELETE') {
      const filePath = path.join(__dirname, req.url);

      // check that it actually exists
      if (!fs.existsSync(filePath)) {
        res.statusCode = 404;
        res.end('File not found');
        return;
      }

      // if its a file use unlink
      if (fs.lstatSync(filePath).isFile()) {
        fs.unlink(filePath, (err) => {
          if (err) {
            res.statusCode = 404;
            res.end('File not found');
          } else {
            res.end('File deleted');
          }
        });
        // if directory use other delete
      } else if (fs.lstatSync(filePath).isDirectory()) {
        // delete directory at filePath
        fs.rm(filePath, { recursive: true }, (err) => {
          if (err) {
            res.statusCode = 500;
            res.end('Error deleting directory');
          } else {
            res.end('Directory deleted');
          }
        });
      } else {
        res.statusCode = 400;
        res.end('Unable to delete file type');
      }
    } else if (req.method === 'MKCOL') {
      const dirPath = path.join(__dirname, req.url);
      fs.mkdir(dirPath, (err) => {
        if (err) {
          console.log(err.stack);
          res.statusCode = 500;
          res.end('Error creating directory');
        } else {
          res.end('Directory created');
        }
      });
    } else {
      res.statusCode = 405;
      res.end(`The method ${req.method} is not supported.`);
    }
  } catch (e) {
    // Dont expose server internals!
    res.statusCode = 500;
    console.log(e.stack);
    res.end('Server error');
  }
});

server.listen(3000, () => {
  console.log('Server is running on port 3000');
});
