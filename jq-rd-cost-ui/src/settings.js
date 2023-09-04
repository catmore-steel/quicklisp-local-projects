const config =  require('../package.json')
module.exports = {

  title: `加计扣除辅助系统(v${config.version})`,

  /**
   * 是否启用调试模式  true 调试模式 false 线上模式
   */
  isDebug: true,

  /**
   * 是否系统布局配置
   */
  showSettings: false,

  /**
   * 是否显示 tagsView
   */
  tagsView: true,

  /**
   * 是否固定头部
   */
  fixedHeader: false,

  /**
   * 是否显示logo
   */
  sidebarLogo: true,

  /**
   * 是否显示顶部导航
   */
  topNav: false,

  /**
   * 全局默认尺寸  default medium small mini
   */
  size: 'mini',

  /**
   * @type {string | array} 'production' | ['production', 'development']
   * @description Need show err logs component.
   * The default is only used in the production env
   * If you want to also use it in dev, you can pass ['production', 'development']
   */
  errorLog: 'production'
}
