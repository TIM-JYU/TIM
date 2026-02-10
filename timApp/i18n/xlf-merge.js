/*jshint esversion: 9 */
/*jshint node: true */
"use strict";

const convert = require("xml-js");
const fs = require("fs");

const LOCALES = ["fi", "sv", "es", "it"];

function read(path, translationsList) {
    const messagesXLIF = fs.readFileSync(path, {encoding: "utf8"});
    const tl = translationsList;
    // The actual translations are inside source and target elements. Those elements must preserve whitespace fully
    // On the other hand, we would like to format our XML file
    // Because of that, we grab and replace the translations with an ID
    // Later, we insert translations back into formatted XML
    const messagesStr = messagesXLIF
        .replace(/<source>((.|\s)+?)<\/source>/g, (match, m1) => {
            const i = tl.push(m1) - 1;
            return `<source>${i.toString()}</source>`;
        })
        .replace(
            /<target state="([^"]*)">((.|\s)*?)<\/target>/g,
            (match, m1, m2) => {
                const i = tl.push(m2) - 1;
                return `<target state="${m1}">${i.toString()}</target>`;
            }
        );
    // Don't use compact to ensure order is preserved
    const messages = convert.xml2js(messagesStr, {compact: false, trim: true});

    const transUnits = messages.elements[0].elements[0].elements[0].elements;
    const itemSources = new Map();
    const items = new Map();
    const itemKeys = new Set();

    for (const unit of transUnits) {
        items.set(unit.attributes.id, unit);
        itemSources.set(unit.attributes.id, unit.elements[0]);
        itemKeys.add(unit.attributes.id);
    }

    return {
        messages,
        transUnits,
        items,
        itemSources,
        itemKeys,
    };
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

function addTarget(item) {
    const target = {
        ...item.elements[0],
        name: "target",
        attributes: {state: "new"},
    };
    item.elements.splice(1, 0, target);
    return item;
}

const htmlEntities = {
    nbsp: " ",
    cent: "¢",
    pound: "£",
    yen: "¥",
    euro: "€",
    copy: "©",
    reg: "®",
    lt: "<",
    gt: ">",
    quot: '"',
    amp: "&",
    apos: "'",
};

// Adapted from https://stackoverflow.com/a/39243641
function decodeEntities(str) {
    return str.replace(/&([^;]+);/g, (entity, entityCode) => {
        let match;
        if (entityCode in htmlEntities) {
            return htmlEntities[entityCode];
        } else if ((match = entityCode.match(/^#x([\da-fA-F]+)$/))) {
            return String.fromCharCode(parseInt(match[1], 16));
        } else if ((match = entityCode.match(/^#(\d+)$/))) {
            return String.fromCharCode(parseInt(match[1], 10));
        } else {
            return entity;
        }
    });
}

function hashTranslation(text) {
    return decodeEntities(text).replace(/\s/g, "");
}

function mergeTranslations(locale) {
    console.log(`Merging main with "${locale}"`);
    const translationsList = [];
    const newMessages = read("messages.xlf", translationsList);
    const messagesFi = read(`i18n/messages.${locale}.xlf`, translationsList);

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
        const srcText = hashTranslation(
            translationsList[+src.elements[0].text]
        );
        const newSrc = hashTranslation(
            translationsList[
                +newMessages.itemSources.get(msgId).elements[0].text
            ]
        );
        if (srcText === newSrc) {
            continue;
        }
        console.log(`Updated translation: ${msgId}`);
        const newItem = newMessages.items.get(msgId);
        const itemIndex = messagesFi.transUnits.findIndex(
            (i) => i.attributes.id === msgId
        );
        messagesFi.transUnits[itemIndex] = addTarget(newItem);
    }

    const newTlStr = convert.js2xml(messagesFi.messages, {
        compact: false,
        spaces: 2,
        textFn: (value, elName) => {
            if (elName === "source" || elName === "target") {
                return translationsList[+value];
            }
            return value;
        },
    });
    fs.writeFileSync(`i18n/messages.${locale}.xlf`, newTlStr, {
        encoding: "utf8",
    });
    console.log("Done merging translations");
}

for (const locale of LOCALES) {
    mergeTranslations(locale);
}
