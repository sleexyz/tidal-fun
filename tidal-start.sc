s.options.numBuffers = 1024 * 16;
s.options.memSize = 1024 * 512;

s.waitForBoot {
  Routine {
    include("SuperDirt");
    2.wait;
    SuperDirt.start;
  }.play
};
// s.dumpOSC();
