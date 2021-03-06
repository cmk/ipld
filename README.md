[![CircleCI](https://circleci.com/gh/cmk/ipld.svg?style=svg&circle-token=2cd535340f7099b5d5b988423cefbc48494d444f)](https://circleci.com/gh/cmk/ipld)

`ipld` is a recursion schemes library for operating on hash-linked data structures such as those found in VCSs (e.g. Git, Darcs, Mercurial, etc.), blockchains (e.g. Bitcoin, Ethereum, etc), and distributed stores (e.g. IPFS, Riak, etc.). See the [IPLD specification](https://github.com/ipld/specs) for more details.

At present the main purpose of `ipld` is as a platform for research in Merkle graph specification, construction, and interop, in particular research into compiling DAG-based expressions such as [legit](https://morr.cc/legit/), [IPLD selectors](https://github.com/ipld/specs/blob/master/selectors/selectors.md), or [Merklized ASTs](http://www.mit.edu/~jlrubin/public/pdfs/858report.pdf).
