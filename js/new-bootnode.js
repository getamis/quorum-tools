const acctUtil = require('./account-utils');
var bootNode = acctUtil.newKeyPair();
var istanbulValidator = {
    "nodeKey":bootNode.private,
    "enode":bootNode.public,
    "address":bootNode.address
};

console.log(JSON.stringify(istanbulValidator));
