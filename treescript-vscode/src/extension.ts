'use strict'

import * as vscode from 'vscode';
import Client from './client';
import { Service } from './interfaces';

let services: Service[]

export async function activate(context: vscode.ExtensionContext) {
  services = [
    new Client(context)
    //new Decorator(context)
  ]

  services.forEach(service => {
    service.begin()
  })
}

/*
 * Deactivate each of the LSP servers.
 */
export async function deactivate() {
  services.forEach(service => {
    service.end()
  })
}
