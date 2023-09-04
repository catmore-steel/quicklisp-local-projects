<template>
  <div>
    <el-menu
      style="float: left;"
      :default-active="activeMenu"
      mode="horizontal"
      @select="handleSelect">
      <template v-for="(item, index) in topMenus">
        <el-menu-item :style="{'--theme': theme}" :index="item.path" :key="index" v-if="index < visibleNumber">
          <item v-if="item.meta" :icon="item.meta && item.meta.icon" :title="item.meta.title"/>
        </el-menu-item>
      </template>
      <!-- 顶部菜单超出数量折叠 -->
      <el-submenu :style="{'--theme': theme}" index="more" v-if="topMenus.length > visibleNumber">
        <template slot="title">更多菜单</template>
        <template v-for="(item, index) in topMenus">
          <el-menu-item :index="item.path" :key="index" v-if="index >= visibleNumber">
            <svg-icon :icon-class="item.meta.icon"/>
            {{ item.meta.title }}
          </el-menu-item>
        </template>
      </el-submenu>
    </el-menu>

    <div v-if="showCompanyItem" class=""
         style="margin-right: 50px;display: flex;float: right;height: 100%;line-height: 50px;margin-left: 150px;">
      <div class="default-company-name">客户名称:</div>
      <el-select v-model="companyId" placeholder="撰写默认客户名称" size="medium" style="width: 200px"
                 @change="changeCompanySelect" :clearable="false">
        <el-option
          v-for="dict in companyOptions"
          :key="dict.id"
          :label="dict.companyName"
          :value="dict.id"
        ></el-option>
      </el-select>
      <div class="default-company-name">申报项目:</div>
      <el-select v-model="itemNo" placeholder="撰写默认项目名称" size="medium" style="width: 200px"
                 @change="changeItemSelect" :clearable="false">
        <el-option
          v-for="dict in itemOptions"
          :key="dict.itemNo"
          :label="dict.itemName"
          :value="dict.itemNo"
        ></el-option>
      </el-select>
    </div>
    <notify />
    <ai-chat />
  </div>
</template>

<script>
import { constantRoutes } from '@/router'
import Item from '@/layout/components/Sidebar/Item'
import notify from '@/components/TopNav/notify'
import aiChat from '@/components/TopNav/aiChat'
import { findListCompanyinfo } from '@/api/project/companyinfo'
import { projectBaseItemInfoFindList } from '@/api/project/baseItemInfo';
// 隐藏侧边栏路由
const hideList = ['/index', '/user/profile']

export default {
  components: {
    Item,
    notify,
    aiChat
  },
  data() {
    return {
      // 顶部栏初始数
      showCompanyItem: false,
      visibleNumber: 5,
      // 当前激活菜单的 index
      currentIndex: undefined,
      itemOptions: [],
      companyId: null,
      itemNo: null,
      companyOptions: []
    }
  },
  created() {
    this.getCompanyList();
  },
  computed: {
    theme() {
      return this.$store.state.settings.theme
    },
    // 顶部显示菜单
    topMenus() {
      let topMenus = []
      this.routers.map((menu) => {
        if (menu.hidden !== true) {
          // 兼容顶部栏一级菜单内部跳转
          if (menu.path === '/') {
            topMenus.push(menu.children[0])
          } else {
            topMenus.push(menu)
          }
        }
      })
      return topMenus
    },
    // 所有的路由信息
    routers() {
      return this.$store.state.permission.topbarRouters
    },
    // 设置子路由
    childrenMenus() {
      var childrenMenus = []
      this.routers.map((router) => {
        for (var item in router.children) {
          if (router.children[item].parentPath === undefined) {
            if (router.path === '/') {
              router.children[item].path = '/' + router.children[item].path
            } else {
              if (!this.ishttp(router.children[item].path)) {
                router.children[item].path = router.path + '/' + router.children[item].path
              }
            }
            router.children[item].parentPath = router.path
          }
          childrenMenus.push(router.children[item])
        }
      })
      return constantRoutes.concat(childrenMenus)
    },
    // 默认激活的菜单
    activeMenu() {
      const path = this.$route.path
      let activePath = path
      if (path !== undefined && path.lastIndexOf('/') > 0 && hideList.indexOf(path) === -1) {
        const tmpPath = path.substring(1, path.length)
        activePath = '/' + tmpPath.substring(0, tmpPath.indexOf('/'))
        this.$store.dispatch('app/toggleSideBarHide', false)
      } else if (!this.$route.children) {
        activePath = path
        this.$store.dispatch('app/toggleSideBarHide', true)
      }
      if (path !== undefined && path.indexOf('/cost/') > -1) {
        this.showCompanyItem = true
      } else {
        this.showCompanyItem = false
      }
      // console.log("activeMenu");
      this.activeRoutes(activePath)
      return activePath
    }
  },
  beforeMount() {
    window.addEventListener('resize', this.setVisibleNumber)
  },
  beforeDestroy() {
    window.removeEventListener('resize', this.setVisibleNumber)
    this.EventBus.$off("resetCompanyArr")
  },
  mounted() {
    this.setVisibleNumber()

  },
  methods: {
    getCompanyList(){
      findListCompanyinfo().then(response => {
        this.companyOptions = response.data.filter(elt=>elt.baseItemInfos && elt.baseItemInfos.length>0);
        this.companyId = this.companyOptions[0].id
        this.$store.commit('SET_COMPANY_ID', this.companyId)
        this.changeCompanySelect(this.companyId)
      })
    },
    changeCompanySelect(companyId) {
      this.itemOptions = []
      this.itemNo = null
      this.$store.commit('SET_COMPANY_ID', companyId)
      projectBaseItemInfoFindList(companyId).then(response => {
        this.itemOptions = response.data
        if (this.itemOptions.length > 0) {
          this.itemNo = this.itemOptions[0].itemNo
        }
        this.$store.commit('SET_ITEM_NO', this.itemNo)
      })

    },
    changeItemSelect(itemNo) {
      this.$store.commit('SET_ITEM_NO', itemNo)
    },
    goToItemFile() {
      this.$router.push('/write/output/itemFile?menuKey=2798')
    },
    // 根据宽度计算设置显示栏数
    setVisibleNumber() {
      const width = document.body.getBoundingClientRect().width / 3
      this.visibleNumber = parseInt(width / 85)
    },
    // 菜单选择事件
    handleSelect(key, keyPath) {
      this.currentIndex = key
      const route = this.routers.find(item => item.path === key)
      if (this.ishttp(key)) {
        // http(s):// 路径新窗口打开
        window.open(key, '_blank')
      } else if (!route || !route.children) {
        // 没有子路由路径内部打开
        this.$router.push({ path: key })
        //影藏SiderBar
        this.$store.dispatch('app/toggleSideBarHide', true)
      } else {
        // 显示左侧联动菜单
        // console.log("handleSelect");
        // console.log(key);
        this.activeRoutes(key)
        //并加载子路由
        let firstMenu = route.children[0].path + '/' + route.children[0].children[0].path + '?menuKey=' + route.children[0].children[0].menuId
        this.$router.push({ path: firstMenu })

        //显示SiderBar
        // this.$store.dispatch('app/toggleSideBarHide', false);
      }
    },
    // 当前激活的路由
    activeRoutes(key) {
      var routes = []
      if (this.childrenMenus && this.childrenMenus.length > 0) {
        this.childrenMenus.map((item) => {
          if (key == item.parentPath || (key == 'index' && '' == item.path)) {
            routes.push(item)
          }
        })
      }
      if (routes.length > 0) {
        // console.log(key);
        // console.log(routes);
        this.$store.commit('SET_SIDEBAR_ROUTERS', routes)
      }
    },
    ishttp(url) {
      return url.indexOf('http://') !== -1 || url.indexOf('https://') !== -1
    }
  }
}
</script>

<style lang="scss" scoped>
.el-menu-item {
  width: auto;
}

.topmenu-container {
  .el-menu--horizontal > .el-menu-item {
    float: left;
    height: 50px !important;
    line-height: 50px !important;
    color: #999093 !important;
    padding: 0 5px !important;
    margin: 0 10px !important;
  }

  .el-menu--horizontal > .el-menu-item.is-active, .el-menu--horizontal > .el-submenu.is-active .el-submenu__title {
    border-bottom: 2px solid #409eff !important;;
    color: #303133;
  }

  .el-menu--horizontal > .el-submenu .el-submenu__title {
    float: left;
    height: 50px !important;
    line-height: 50px !important;
    color: #999093 !important;
    padding: 0 5px !important;
    margin: 0 10px !important;
  }

  .default-company-name {
    width: 100px;
    font-size: 14px;
    text-align: center;
    color: #303133;
  }
}


/* submenu item */
</style>
