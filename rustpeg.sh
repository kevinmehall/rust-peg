#!/bin/bash
set -e

node_modules/.bin/pegjs bootstrap.pegjs;
node > grammar_def.rs <<- EOF
	var parser = require('./bootstrap'),
	    fs     = require('fs')

	var source = fs.readFileSync("$1", {encoding:'utf8'})
	console.log(parser.parse(source))
EOF

rust run peg.rs