// Test script with IIFE that should return a value
(() => {
    function findElement() {
        const el = document.querySelector('body');
        if (!el) {
            return { ok: false, reason: 'no-body' };
        }
        return { ok: true, tag: el.tagName };
    }
    
    const result = findElement();
    return result;
})()