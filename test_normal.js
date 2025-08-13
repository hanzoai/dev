// Test script without IIFE that should also return a value
function findElement() {
    const el = document.querySelector('body');
    if (!el) {
        return { ok: false, reason: 'no-body' };
    }
    return { ok: true, tag: el.tagName };
}

const result = findElement();
result