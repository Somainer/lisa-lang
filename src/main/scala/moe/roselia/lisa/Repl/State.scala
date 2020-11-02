package moe.roselia.lisa.Repl

import moe.roselia.lisa.Environments

case class State(resultIndex: Int,
                 environment: Environments.Environment)
