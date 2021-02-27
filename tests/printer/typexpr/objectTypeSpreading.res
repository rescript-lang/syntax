type a = {"x": int}
type u = {...a, "u": int}
type v = {"v": int, ...a}
type w = {"j": int, ...a, "k": int, ...v}
