let react = require('react');
let rdom = require('react-dom');
let expect = require('chai').expect;

let reactix = require('../src/Reactix.purs');
let dom = require('../src/Reactix/DOM.purs');
let test = require('./ReactixTest.purs');

// console.log("reactix: ", reactix);
// console.log("dom: ", dom);
// console.log("test: ", test);

let root = dom.findRootById("existing")().value0;

expect(root).to.be.a("HTMLDivElement");

let raf = (x) => window.requestAnimationFrame(x);
let wait = async () =>
  new Promise((resolve) => {
    raf(() => raf(() => raf(() => raf(() => raf(() => raf(() => raf(() => resolve(true))))))));
  });

describe('purescript dom test 1', () => {
  
});
//We hope this will be enough time for react to render reliably

let mkref = () => test.mkRef(0)();

let psDomTest = async (count) => {
  test.dom1Test()();
  expect(root.children.length).to.equal(1);
  expect(root.children[0].tagName).to.equal("I");
  expect(root.children[0].innerHTML).to.equal("Hello World");
  return count + 3;
};

let psFragTest = async (count) => {
  test.frag1Test()();
  expect(root.children.length).to.equal(2);
  expect(root.children[0].tagName).to.equal("I");
  expect(root.children[0].innerHTML).to.equal("Hello World");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("Hello World");
  return count + 5;
};

let psStaticCounterTest = async (count) => {
  let r = mkref();
  test.staticCounter1Test()(r)();
  await wait();
  expect(r.value).to.be.above(1);
  expect(root.children.length).to.equal(2);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("1");
  await wait();
  expect(r.value).to.be.above(1);
  return count + 7;
};

let psStaticMemoCounterTest = async (count) => {
  let r = mkref();
  test.staticCounter2Test()(r)();

  await wait();
  expect(r.value).to.equal(1);
  expect(root.children.length).to.equal(2);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("1");

  await wait();
  expect(r.value).to.equal(1);
  return count + 7;
};
let psStaticMemo_CounterTest = async (count) => {
  let r = mkref();
  test.staticCounter3Test()(r)();

  await wait();
  expect(r.value).to.equal(1);
  expect(root.children.length).to.equal(2);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("1");

  await wait();
  expect(r.value).to.equal(1);
  return count + 7;
};

let psLiveCounterTest = async (count) => {
  let r = mkref();
  test.liveCounter1Test()(r)();

  await wait();
  expect(r.value).to.be.above(2);
  expect(root.children.length).to.equal(3);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("1");
  expect(root.children[2].tagName).to.equal("I");
  expect(root.children[2].innerHTML).to.equal("999");

  root.children[0].click();

  await wait();
  expect(r.value).to.be.above(4);
  expect(root.children.length).to.equal(3);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("2");
  expect(root.children[2].tagName).to.equal("I");
  expect(root.children[2].innerHTML).to.equal("999");

  await wait();
  expect(r.value).to.be.above(6);
  return count + 17;
};
let psLiveCounterMemoTest = async (count) => {
  let r = mkref();
  test.liveCounter2Test()(r)();

  await wait();
  expect(r.value).to.equal(2);
  expect(root.children.length).to.equal(3);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("1");
  expect(root.children[2].tagName).to.equal("I");
  expect(root.children[2].innerHTML).to.equal("999");

  root.children[0].click();

  await wait();
  expect(r.value).to.equal(3);
  expect(root.children.length).to.equal(3);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("2");
  expect(root.children[2].tagName).to.equal("I");
  expect(root.children[2].innerHTML).to.equal("999");

  await wait();
  expect(r.value).to.equal(3);
  return count + 17;
};
let psLiveCounterMemo_Test = async (count) => {
  let r = mkref();
  test.liveCounter3Test()(r)();

  await wait();
  expect(r.value).to.equal(2);
  expect(root.children.length).to.equal(3);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("1");
  expect(root.children[2].tagName).to.equal("I");
  expect(root.children[2].innerHTML).to.equal("999");

  root.children[0].click();

  await wait();
  expect(r.value).to.equal(3);
  expect(root.children.length).to.equal(3);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("2");
  expect(root.children[2].tagName).to.equal("I");
  expect(root.children[2].innerHTML).to.equal("999");

  await wait();
  expect(r.value).to.equal(3);
  return count + 17;
};

let jsDomTest = async (count) => {
  dom.render(test.dom1)(root)();
  expect(root.children.length).to.equal(1);
  expect(root.children[0].tagName).to.equal("I");
  expect(root.children[0].innerHTML).to.equal("Hello World");
  return count + 3;
};

let jsFragTest = async (count) => {
  dom.render(test.frag1)(root)();
  expect(root.children.length).to.equal(2);
  expect(root.children[0].tagName).to.equal("I");
  expect(root.children[0].innerHTML).to.equal("Hello World");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("Hello World");
  return count + 5;
};

let jsStaticCounterTest = async (count) => {
  var data = {count: 1};
  let r = mkref();
  let ctr = dom.effComponent(test.staticCounter1(r))(data)([])();
  dom.render(ctr)(root)();

  await wait();
  expect(r.value).to.be.above(1);
  expect(root.children.length).to.equal(2);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("1");

  await wait();
  expect(r.value).to.be.above(2);

  return count + 7;
};

let jsStaticMemoCounterTest = async (count) => {
  var data = {count: 1};
  let r = mkref();
  let c0 = reactix.memo(test.staticCounter0(r))(test.cmp);
  let c1 = dom.effComponent(test.staticCounter2(c0))(data)([])();
  dom.render(c1)(root)();

  await wait();
  expect(r.value).to.equal(1);
  expect(root.children.length).to.equal(2);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("1");

  await wait();
  expect(r.value).to.equal(1);
  return count + 7;
};
let jsStaticMemo_CounterTest = async (count) => {
  var data = {count: 1};
  let r = mkref();
  let c0 = reactix.memo_(test.staticCounter0(r));
  let c1 = dom.effComponent(test.staticCounter2(c0))(data)([])();
  dom.render(c1)(root)();

  await wait();
  expect(r.value).to.equal(1);
  expect(root.children.length).to.equal(2);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("1");

  await wait();
  expect(r.value).to.equal(1);
  return count + 7;
};

let jsLiveCounterTest = async (count) => {
  var data = {count: 1};
  let r = mkref();
  let ctr = dom.effComponent(test.liveCounter1(r))(data)([])();
  dom.render(ctr)(root)();

  await wait();
  expect(r.value).to.be.above(2);
  expect(root.children.length).to.equal(3);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("1");
  expect(root.children[2].tagName).to.equal("I");
  expect(root.children[2].innerHTML).to.equal("999");

  root.children[0].click();

  await wait();
  expect(r.value).to.be.above(4);
  expect(root.children.length).to.equal(3);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("2");
  expect(root.children[2].tagName).to.equal("I");
  expect(root.children[2].innerHTML).to.equal("999");

  await wait();
  expect(r.value).to.be.above(6);
  return count + 13;
};
let jsLiveCounterMemoTest = async (count) => {
  var data = {count: 1};
  let r = mkref();
  let c = test.lc2(r);
  let ctr = dom.effComponent(test.liveCounter2(c))(data)([])();
  dom.render(ctr)(root)();

  await wait();
  expect(r.value).to.equal(2);
  expect(root.children.length).to.equal(3);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("1");
  expect(root.children[2].tagName).to.equal("I");
  expect(root.children[2].innerHTML).to.equal("999");

  root.children[0].click();

  await wait();
  expect(r.value).to.equal(3);
  expect(root.children.length).to.equal(3);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("2");
  expect(root.children[2].tagName).to.equal("I");
  expect(root.children[2].innerHTML).to.equal("999");

  await wait();
  expect(r.value).to.equal(3);
  return count + 17;
};
let jsLiveCounterMemo_Test = async (count) => {
  var data = {count: 1};
  let r = mkref();
  let c = test.lc3(r);
  let ctr = dom.effComponent(test.liveCounter3(c))(data)([])();
  dom.render(ctr)(root)();

  await wait();
  expect(r.value).to.equal(2);
  expect(root.children.length).to.equal(3);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("1");
  expect(root.children[2].tagName).to.equal("I");
  expect(root.children[2].innerHTML).to.equal("999");

  root.children[0].click();

  await wait();
  expect(r.value).to.equal(3);
  expect(root.children.length).to.equal(3);
  expect(root.children[0].tagName).to.equal("BUTTON");
  expect(root.children[0].innerHTML).to.equal("++");
  expect(root.children[1].tagName).to.equal("I");
  expect(root.children[1].innerHTML).to.equal("2");
  expect(root.children[2].tagName).to.equal("I");
  expect(root.children[2].innerHTML).to.equal("999");

  await wait();
  expect(r.value).to.equal(3);
  return count + 17;
};

let psTests = async (count) =>
    psDomTest(count)
    .then(psFragTest)
    .then(psStaticCounterTest)
    .then(psStaticMemoCounterTest)
    .then(psStaticMemo_CounterTest)
    .then(psLiveCounterTest)
    .then(psLiveCounterMemoTest)
    .then(psLiveCounterMemo_Test)
;

let jsTests = async (count) =>
    jsDomTest(count)
    .then(jsFragTest)
    .then(jsStaticCounterTest)
    .then(jsStaticMemoCounterTest)
    .then(jsStaticMemo_CounterTest)
    .then(jsLiveCounterTest) 
    .then(jsLiveCounterMemoTest)
    .then(jsLiveCounterMemo_Test)
;

psTests(0).then(jsTests)
  .then((count) => console.log("Tests passed: ", count)); 
