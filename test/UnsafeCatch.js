"use strict";

exports.unsafeCatchPurely = (fn) => {
    return (error_message) => {
        try {
            return fn()
        }
        catch (e) {
            return error_message(e.message)
        } 
    }

}
