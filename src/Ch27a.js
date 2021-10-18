const chalk = require("chalk");

exports._chalk = (styles, str) => {
    let c = chalk;
    for (let i = 0; i < styles.length; ++i){
        c = c[styles[i]];
    }

    return c(str);
}