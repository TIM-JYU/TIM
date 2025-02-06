import {Component, NgModule, OnInit} from "@angular/core";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClientModule} from "@angular/common/http";
import {AngularPluginBase} from "../plugin/angular-plugin-base.directive";
import * as t from "io-ts";
import {getTopLevelFields} from "../plugin/attributes";
