// http://stackoverflow.com/questions/25803420/how-to-compile-clojurescript-to-nodejs
try {
    require("source-map-support").install();
} catch(err) {
}
require("./target/goog/bootstrap/nodejs.js");
require("./target/emulator-chip8.js");
require("./target/emulator_chip8/core");
//emulator_chip8.core._main(process.argv[2]); // passing argument
emulator_chip8.core._main(); // passing argument