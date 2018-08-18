const js = import("./vfin_dom");

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
