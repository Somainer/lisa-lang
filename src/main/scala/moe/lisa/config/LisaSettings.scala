package moe.lisa.config

import dotty.tools.dotc.config.Settings.SettingGroup

trait LisaCommonSettings { self: SettingGroup =>

}

class LisaSettings extends SettingGroup, LisaCommonSettings
