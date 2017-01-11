CodeMirror.defineSimpleMode("sysml", {
    start: [
        { regex: /(?:bdd|package|block|references|constraints|inv|not|operations|owned\s+behaviors|ports|subsets|state\s+machine|(?:initial\s+)?state|receive|after|pre|post|derive)\b/, token: "keyword" },
        { regex: /--.*/, token: "comment" }
    ],
    meta: {
        lineComment: "--"
    }
})