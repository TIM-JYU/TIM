import * as aceimp from "ace-builds/src-noconflict/ace";
import * as langtools from "ace-builds/src-noconflict/ext-language_tools";
import * as r from "ace-builds/webpack-resolver";
import {markAsUsed} from "tim/util/utils";

markAsUsed(langtools, r);

export const ace = aceimp;
export type IAce = typeof ace;
