"use strict";

exports.unsafeCatch = (fn) => {
    return (fn2) => {
        try {
            return fn2()
        }
        catch (e) {
            return fn(e)
        } 
    }

}
