'use strict'

var _  = require('underscore');
var crypto = require('crypto');
var ethUtil = require('ethereumjs-util');

var account = {
    formatAddress:function(addr, format){
        if(_.isUndefined(format) || !_.isString(format))
            format = 'hex';

        if(_.isUndefined(addr)
            || !_.isString(addr))
            addr = '0000000000000000000000000000000000000000';

        if(addr.substr(0, 2) == '0x' && format == 'raw')
            addr = addr.substr(2);

        if(addr.substr(0, 2) != '0x' && format == 'hex')
            addr = '0x' + addr;

        return addr;
    },
    randomBytes: function(length) {
        var charset = "abcdef0123456789";
        var i;
        var result = "";
        var values = crypto.randomBytes(length);
        for (i = 0; i < length; i++) {
            result += charset[values[i] % charset.length];
        }
        return result;
    },
    newKeyPair: function(){
        var priv = new Buffer(account.randomBytes(64), 'hex');
        var pub = ethUtil.privateToPublic(priv);
        return {
            private: priv.toString('hex'),
            public: pub.toString('hex'),
            address: account.formatAddress(ethUtil.publicToAddress(pub).toString('hex'))
        };
    }
};

module.exports = account;
