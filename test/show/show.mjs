// A block in Peregrine's WebAssembly is `[tag, arg₁ … argₙ]` and does not record
// n, so the value is read following the shape of another target's value (argv[3]).
import {readFileSync} from 'fs';

const [file, guide] = process.argv.slice(2);

// `<fun>`, `null` for an immediate, or the shapes of a block's arguments
const shape = s => {
  const ts = s?.match(/\(|\)|<fun>|\d+/g) ?? [];
  let i = 0;
  const go = () => {
    const t = ts[i++];
    if (t !== '(') return t === '<fun>' ? t : null;
    const args = [];
    for (i++; i < ts.length && ts[i] !== ')'; ) args.push(go());
    i++;
    return args;
  };
  return go();
};

const env = {
  write_char: c => process.stdout.write(String.fromCharCode(c)),
  write_int: n => process.stdout.write(`${n}`)
};
const {instance: {exports: x}} =
  await WebAssembly.instantiate(readFileSync(file), {env});
x.main_function();
if (x.out_of_mem.value) throw new Error('out of memory');

const mem = new DataView(x.memory.buffer);
const word = p => mem.getInt32(p, true);
const field = (v, i) => word(v + 4 * (i + 1));
const show = (v, s) =>
  s === '<fun>' ? s
  : v & 1 ? `${v >> 1}`
  : `(${[word(v), ...(s ?? []).map((a, i) => show(field(v, i), a))].join(' ')})`;

console.log(show(x.result.value, shape(guide)));
