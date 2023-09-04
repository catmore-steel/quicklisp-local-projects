<template>
    <div :class="{'has-logo':showLogo}">
        <logo v-if="showLogo" :collapse="isCollapse" />
        <el-scrollbar wrap-class="scrollbar-wrapper">
            <el-menu
                :default-active="activeMenu"
                :collapse="isCollapse"
                :background-color="variables.menuBg"
                :text-color="variables.menuText"
                :unique-opened="true"
                :active-text-color="settings.theme"
                :collapse-transition="false"
                mode="vertical"
            >
                <sidebar-item
                    class="sidebar-padding"
                    v-for="(route, index) in sidebarRouters"
                    :key="route.path  + index"
                    :item="route"
                    :base-path="route.path"
                />
            </el-menu>
        </el-scrollbar>
    </div>
</template>

<script>
import { mapGetters, mapState } from "vuex";
import Logo from "./Logo";
import SidebarItem from "./SidebarItem";
import variables from "@/assets/styles/variables.scss";

export default {
    components: { SidebarItem, Logo },
  created() {
    // 左侧的 nav默认展示的 是顶部nav的第一个子项
    let sideNav = []
    this.sidebarRouters.forEach((menu) => {
      if (menu.hidden !== true) {
        if (menu.path === "/") {
          sideNav.push(menu.children[0]);
        } else {
          if(!menu.path) return
          sideNav.push(menu);
        }
      }
    })
    if(sideNav.length > 1){
      this.$store.commit("SET_SIDEBAR_ROUTERS", sideNav[0].children);
    }
  },
    computed: {
        ...mapState(["settings"]),
        ...mapGetters(["sidebarRouters", "sidebar"]),
        activeMenu() {
            const route = this.$route;
            const { meta, path } = route;
            // if set path, the sidebar will highlight the path you set
            if (meta.activeMenu) {
                return meta.activeMenu;
            }
            return path;
        },
        showLogo() {
            return this.$store.state.settings.sidebarLogo;
        },
        variables() {
            return variables;
        },
        isCollapse() {
            return !this.sidebar.opened;
        }
    }
};
</script>


<style scoped>

.sidebar-padding >>> .router-link-exact-active .el-menu-item{
  padding-left: 20px !important;
}

.sidebar-padding >>> .el-submenu .el-submenu__title{
  padding-left: 20px !important;
}
.sidebar-padding >>> .el-menu .nest-menu .el-menu-item{
  padding-left: 25px !important;
}
.sidebar-padding >>> .el-menu .nest-menu .el-submenu__title{
  padding-left: 25px !important;
}
.sidebar-padding >>> .el-menu  .nest-menu .el-menu .el-menu-item {
  padding-left: 30px !important;
}

</style>
