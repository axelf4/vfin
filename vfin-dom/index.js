const js = import("./js_hello_world");

export function exit_with_live_runtime() {
	throw "SimulateInfiniteLoop";
}

js.then(js => {
	try {
		js.run();
	} catch (e) {
		if (e !== "SimulateInfiniteLoop") throw e;
	}
});
