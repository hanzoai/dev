#!/usr/bin/env node
/*
 Upgrade codex-rs/tui/assets/spinners.json to include human labels and groups.
 Usage:
   node scripts/upgrade-spinners.js            # writes in place
   node scripts/upgrade-spinners.js --dry-run  # prints a summary only
*/
const fs = require('fs');
const path = require('path');

const file = path.join(__dirname, '..', 'codex-rs', 'tui', 'assets', 'spinners.json');

function humanize(name) {
  // camelCase / kebab-case / snake_case → Title Case with digits spaced
  let out = '';
  let prevLower = false;
  let prevAlpha = false;
  for (const ch of name) {
    if (ch === '-' || ch === '_') { out += ' '; prevLower = false; prevAlpha = false; continue; }
    if (/[A-Z]/.test(ch) && prevLower) out += ' ';
    else if (/[0-9]/.test(ch) && prevAlpha) out += ' ';
    out += ch;
    prevLower = /[a-z]/.test(ch);
    prevAlpha = /[a-zA-Z]/.test(ch);
  }
  return out.split(/\s+/).filter(Boolean).map(w => w[0].toUpperCase() + w.slice(1).toLowerCase()).join(' ');
}

function groupFor(name) {
  const key = name.toLowerCase();
  if (key.includes('dot')) return 'Dots';
  if (key.includes('circle') || key.includes('round') || key.includes('arc')) return 'Circles';
  if (key.includes('line') || key.includes('pipe') || key.includes('bar') || key.includes('pulse')) return 'Lines';
  if (key.includes('bounce') || key.includes('ball') || key.includes('pong')) return 'Bouncing';
  if (key.includes('star') || key.includes('asterisk')) return 'Stars';
  if (key.includes('arrow') || key.includes('triangle')) return 'Arrows';
  if (key.includes('box') || key.includes('square')) return 'Boxes';
  if (key.includes('toggle')) return 'Toggles';
  if (key.includes('monkey') || key.includes('earth') || key.includes('moon') || key.includes('weather') || key.includes('smiley') || key.includes('emoji')) return 'Emoji';
  return 'Other';
}

function main() {
  const dryRun = process.argv.includes('--dry-run');
  const text = fs.readFileSync(file, 'utf8');
  /** @type {Record<string, any>} */
  const data = JSON.parse(text);
  let updated = 0, total = 0;
  const out = {};
  for (const [name, v] of Object.entries(data)) {
    total++;
    const hasLabel = v && typeof v === 'object' && 'label' in v;
    const hasGroup = v && typeof v === 'object' && 'group' in v;
    const label = hasLabel ? v.label : humanize(name);
    const group = hasGroup ? v.group : groupFor(name);
    if (!hasLabel || !hasGroup) updated++;
    out[name] = { interval: v.interval, frames: v.frames, label, group };
  }
  if (dryRun) {
    console.log(`Would update ${updated}/${total} entries (labels/groups).`);
    return;
  }
  fs.writeFileSync(file, JSON.stringify(out, null, 2) + '\n');
  console.log(`Updated ${updated}/${total} entries. Wrote ${path.relative(process.cwd(), file)}`);
}

if (require.main === module) main();

