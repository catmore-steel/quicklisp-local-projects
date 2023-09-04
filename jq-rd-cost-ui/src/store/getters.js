import item from "./modules/item";

const getters = {
  sidebar: state => state.app.sidebar,
  size: state => state.app.size,
  apiHostPath: state => state.app.apiHostPath,
  device: state => state.app.device,
  visitedViews: state => state.tagsView.visitedViews,
  cachedViews: state => state.tagsView.cachedViews,
  token: state => state.user.token,
  avatar: state => state.user.avatar,
  name: state => state.user.name,
  userId: state => state.user.userId,
  introduction: state => state.user.introduction,
  roles: state => state.user.roles,
  permissions: state => state.user.permissions,
  permission_routes: state => state.permission.routes,
  topbarRouters:state => state.permission.topbarRouters,
  sidebarRouters:state => state.permission.sidebarRouters,
  companyId: state => state.item.companyId,
  itemNo: state => state.item.itemNo,
  licenseInfo: state => state.app.licenseInfo
}
export default getters
