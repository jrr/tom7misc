all: rtcgame.js sim-test.js

# TODO: strict net
rtcgame.js: net.ts sim.ts
	tsc -t es2020 --out $@ $^

sim-test.js: sim.ts arcfour.ts ops-tester.ts sim-test.ts
	tsc --strict -t es2020 --out $@ $^

arcfour-test.js: arcfour.ts arcfour-test.ts
	tsc --strict -t es2020 --out $@ $^

controller-test.js: controller.ts controller-test.ts
	tsc --strict -t es2020 --out $@ $^
