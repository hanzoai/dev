// Fixed code for clicking Stripe anchor - no IIFE wrapper
// 1. Define helper functions at the top level.

/**
 * Checks if an element is rendered (has dimensions and is not hidden by CSS).
 * We intentionally ignore its position relative to the viewport during the search.
 */
function isVisible(el) {
  const r = el.getBoundingClientRect();
  const s = getComputedStyle(el);
  return r.width > 5 && r.height > 5 && s.display !== 'none' && s.visibility !== 'hidden';
}

/**
 * Locates the most likely anchor element corresponding to "Stripe".
 */
function findStripeAnchor() {
  // Search for anchor elements containing "Stripe"
  const anchors = Array.from(document.querySelectorAll('a'));
  const cand = [];
  for (const a of anchors) {
    const text = (a.textContent || '').toLowerCase();
    const title = (a.getAttribute('title') || '').toLowerCase();
    const href = (a.getAttribute('href') || '').toLowerCase();

    if (text.includes('stripe') || title.includes('stripe') || href.includes('stripe')) {
      if (isVisible(a)) cand.push(a);
    }

    const img = a.querySelector('img');
    if (img) {
      const alt = (img.alt || '').toLowerCase();
      const src = (img.src || '').toLowerCase();
      if ((alt && alt.includes('stripe')) || (src && src.includes('stripe'))) {
        if (isVisible(a)) cand.push(a);
      }
    }

    const hasStripeClass = a.className && a.className.toString().toLowerCase().includes('stripe');
    if (hasStripeClass && isVisible(a)) cand.push(a);
  }

  if (cand.length) {
    // Sort candidates by position (prefer higher up) and size (penalize small elements)
    cand.sort((a,b)=>{
      const ar = a.getBoundingClientRect();
      const br = b.getBoundingClientRect();
      const aScore = ar.top + (100000/Math.max(10, ar.width*ar.height));
      const bScore = br.top + (100000/Math.max(10, br.width*br.height));
      return aScore - bScore;
    });
    return cand[0];
  }

  // Fallback search
  const els = Array.from(document.querySelectorAll('span,div,strong,p,h1,h2,h3,svg'));
  for (const el of els) {
    const t = (el.textContent||'').toLowerCase();
    if (t.includes('stripe') && isVisible(el)) {
      let cur = el;
      for (let i=0;i<4 && cur;i++) {
        if (cur.tagName === 'A') return cur;
        cur = cur.parentElement;
      }
    }
  }
  return null;
}

// 2. Execute the main logic (No IIFE)

const target = findStripeAnchor();
if (!target) {
  // By using a top-level return, the backend will correctly capture this output.
  return { ok:false, reason:'no-stripe-anchor' };
}

const href = target.getAttribute('href');

// 3. Ensure the element is clickable

// Bring the element into the center of the viewport.
target.scrollIntoView({block:'center', inline:'center'});

// The backend wraps this code in an AsyncFunction, so 'await' is allowed.
// Wait briefly (300ms) for the scroll to complete and the page to settle.
await new Promise(resolve => setTimeout(resolve, 300));

// 4. Final validation before clicking
const r = target.getBoundingClientRect();
if (!(r.bottom > 0 && r.top < innerHeight && r.right > 0 && r.left < innerWidth)) {
    // If the element is still not in the viewport (e.g., obscured by a sticky header), we cannot click it reliably.
    return { ok: false, reason: 'target-not-in-viewport-after-scroll' };
}

// 5. Perform the action and return the result
target.click();

return { ok:true, href, text:(target.textContent||'').trim().slice(0,80) };