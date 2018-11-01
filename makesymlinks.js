#!/usr/bin/env node
const fs = require('fs')
const path = require('path')
const pRoot = (...args) => path.resolve(__dirname, ...args)
const promisify = require('util').promisify
const readFile = promisify(fs.readFile)
const mkdir = promisify(fs.mkdir)
const symlink = promisify(fs.symlink)
const stat = promisify(fs.stat)

const last = arr => arr[Math.max(arr.length - 1, 0)]

const BASE_DIR = __dirname
const HOME_DIR = process.env.HOME

const fileListToTree_insert = (level, [ currSeg, ...segments ]) => {
  let val;
  if (segments.length === 0) {
    if (currSeg === '') {
      return;
    }
    level.set(currSeg, true)
    return true
  } else {
    const nextLevel = level.has(currSeg) ? level.get(currSeg) : new Map()
    fileListToTree_insert(nextLevel, segments)
    level.set(currSeg, nextLevel)
    return nextLevel

  }
}

function fileListToTree(list) {
  const files = list.sort().map(f => f.split('/'))
  let tree = new Map()
  for (const file of files) {
    fileListToTree_insert(tree, file)
  }
  return tree
}

function readFileList(pth) {
  return (await readFile(pth))
    .toString()
    .split('\n')
    .map(line => line.trim())
    .filter(line => line && !/^#/.test(line))
}

async function link(fileTree, pth) {
  for (const [ segment, next ] of fileTree) {
    if (next instanceof Map) {
      const dirPathDotfiles = path.resolve(BASE_DIR, pth, segment)
      const dirPathHome = path.resolve(HOME_DIR, pth, segment)
      if (!(await stat(dirPathHome)).isDirectory()) {
      if ((await stat(dirPathDotfiles)).isDirectory()) {
        await mkdir(dirPath)
      }
      await link(next, path.join(pth, segment))
    } else {
    }
  }
}

async function main() {
  const toSymlink = readFileList(pRoot('to_symlink'))
  const fileTree = fileListToTree(toSymlink)
  link(fileTree)

}

main().catch(err => console.error(err))
