all: rtcgame.js sim-test.js

rtcgame.js: net.ts sim.ts
	tsc -t es2020 --out $@ $^

sim-test.js: sim.ts arcfour.ts ops-tester.ts sim-test.ts
	tsc --strict -t es2020 --out $@ $^
