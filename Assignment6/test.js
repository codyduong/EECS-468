/** 
* EECS 468 Assignment 6
* Author: Cody Duong
* Replit: https://replit.com/@CodyDuong/assignment6#index.js
* Other Sources:
* - https://nodejs.org/api/http.html#class-httpclientrequest
*/

const http = require('http');

async function sendRequest(method, url, data = null) {
  return new Promise((resolve, reject) => {
    const options = {
      method,
      hostname: 'localhost',
      port: 3000,
      path: url,
      headers: {
        'Content-Type': 'text/plain',
      },
    };

    const req = http.request(options, (res) => {
      let responseData = '';

      res.on('data', (chunk) => {
        responseData += chunk;
      });

      res.on('end', () => {
        console.log(`[${method}] ${url}: ${responseData}`);
        resolve(responseData);
      });
    });

    req.on('error', (error) => {
      reject(error);
    });

    if (data) {
      req.write(data);
    }

    req.end();
  });
}

// Usage example
(async () => {
  try {
    const response = await sendRequest('GET', '/file.txt');
    console.log(response);
  } catch (error) {
    console.error(error);
  }
})();

function assert(condition, message = "put a message here") {
  if (!condition) {
    throw new Error(`Assertion failed: ${message}`);
  }
}

/**
 * Test every method with a valid request and invalid request.
 * 
 * For PUT double check we are actually "PUTTING" stuff on the server.
 * 
 * Only PATCH has an invalid request, since it will always be invalid
 */

// use an IIFE i hate nodejs i forgot how to write it
(async () => {
  // Test GET request
  await sendRequest('GET', '/file.txt'); // valid
  await sendRequest('GET', '/nonexistentfile.txt'); // invalid

  // Test PUT request
  await sendRequest('PUT', '/testfile.txt', 'foobar'); // valid
  let file = await sendRequest('GET', '/testfile.txt'); // expect to be valid
  assert(file == 'foobar', "foobar was not found"); // expect this content
  await sendRequest('PUT', '/testfile.txt', 'bazbang'); // What happens?
  file = await sendRequest('GET', '/testfile.txt'); // expect to be valid
  assert(file == 'bazbang', "bazbang was not found"); // expect this content
  await sendRequest('PUT', ''); // invalid

  // Test DELETE request
  await sendRequest('DELETE', '/testfile.txt'); // valid
  await sendRequest('DELETE', '/testfile.txt'); // invalid
  await sendRequest('DELETE', '/newdirectory'); // this test isn't idempotent but its
  // not like we can gurantee idempotency from the client side

  // Test MKCOL request
  await sendRequest('MKCOL', '/newdirectory'); // valid
  await sendRequest('MKCOL', ''); // invalid

  // Test unknown method
  await sendRequest('PATCH', '/somefile.txt');
})()