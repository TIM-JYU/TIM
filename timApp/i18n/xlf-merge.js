/*jshint esversion: 9 */
/*jshint node: true */
"use strict";

const convert = require("xml-js");
const fs = require("fs");

const LOCALES = ["fi"];

function read(path) {
    const messagesStr = fs.readFileSync(path, {encoding: "utf8"});
    // Don't use compact to ensure order is preserved
    const messages = convert.xml2js(messagesStr, { compact: false, trim: true });

    const transUnits = messages.elements[0].elements[0].elements[0].elements;
    const itemSources = new Map();
    const items = new Map();
    const itemKeys = new Set();

    for (const unit of transUnits) {
        items.set(unit.attributes.id, unit);
        itemSources.set(unit.attributes.id, unit.elements[0]);
        itemKeys.add(unit.attributes.id);
    }

    return {messages, transUnits, items, itemSources, itemKeys};
}

function setDiff(setA, setB) {
    const res = new Set();

    for (const item of setA) {
        if (setB.has(item)) {
            continue;
        }
        res.add(item);
    }

    return res;
}

function de(obj1, obj2) {
    if (typeof obj1 !== typeof obj2) {
        return false;
    }
    if (typeof obj1 === "string") {
        return obj1.replace(/\s/g, "") === obj2.replace(/\s/g, "");
    }
    if (obj1 instanceof Array) {
        if (obj1.length !== obj2.length) {
            return false;
        }
        for (let i = 0; i < obj1.length; i++) {
            if (!de(obj1[i], obj2[i])) {
                return false;
            }
        }
    }
    if (obj1 instanceof Object) {
        for (const k of Object.keys(obj1)) {
            if (!obj2.hasOwnProperty(k)) {
                return false;
            }
            if (!de(obj1[k], obj2[k])) {
                return false;
            }
        }
    }
    return true;
}

function addTarget(item) {
    const target = {...item.elements[0], name: "target", attributes: { state: "new" }};
    item.elements.splice(1, 0, target);
    return item;
}


function mergeTranslations(locale) {
    console.log(`Merging main with "${locale}"`);
    const newMessages = read("messages.xlf");
    const messagesFi = read(`i18n/messages.${locale}.xlf`);

    const addedMessages = setDiff(newMessages.itemKeys, messagesFi.itemKeys);
    const removedMessages = setDiff(messagesFi.itemKeys, newMessages.itemKeys);

    for (const newItem of addedMessages) {
        console.log(`New translation: ${newItem}`);
        messagesFi.transUnits.push(addTarget(newMessages.items.get(newItem)));
    }

    for (const removedItem of removedMessages) {
        console.log(`Removed translation: ${removedItem}`);
        const index = messagesFi.transUnits.findIndex(
            (i) => i.attributes.id === removedItem
        );
        messagesFi.transUnits.splice(index, 1);
    }

    for (const [msgId, src] of messagesFi.itemSources.entries()) {
        if (!newMessages.itemKeys.has(msgId)) {
            continue;
        }
        const newSrc = newMessages.itemSources.get(msgId);
        if (de(newSrc, src)) {
            continue;
        }
        console.log(`Updated translation: ${msgId}`);
        const newItem = newMessages.items.get(msgId);
        const itemIndex = messagesFi.transUnits.findIndex(
            (i) => i.attributes.id === msgId
        );
        messagesFi.transUnits[itemIndex] = addTarget(newItem);
    }

    const newTlStr = convert.js2xml(messagesFi.messages, { compact: false, spaces: 2 });
    fs.writeFileSync(`i18n/messages.${locale}.xlf`, newTlStr, {
        encoding: "utf8",
    });
    console.log("Done merging translations");
}

for (const locale of LOCALES) {
    mergeTranslations(locale);
}
