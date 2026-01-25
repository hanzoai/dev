#!/usr/bin/env node
/**
 * CLI Entry Point
 */

import { createCLI } from './index.js'

const cli = createCLI()
cli.parse(process.argv)
