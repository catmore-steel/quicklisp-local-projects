import request from '@/utils/jqRequest'

// 查询角色列表
export function listRole(query) {
  return request({
    url: '/system/role/list',
    method: 'get',
    params: query
  })
}

// 查询角色详细
export function getRole(roleId) {
  return request({
    url: '/system/role/' + roleId,
    method: 'get'
  })
}

// 新增角色
export function addRole(data) {
  return request({
    url: '/system/role',
    method: 'post',
    data: data
  })
}

// 修改角色
export function updateRole(data) {
  return request({
    url: '/system/role',
    method: 'put',
    data: data
  })
}

// 角色数据权限
export function dataScope(data) {
  return request({
    url: '/system/role/dataScope',
    method: 'put',
    data: data
  })
}

// 角色状态修改
export function changeRoleStatus(roleId, status) {
  const data = {
    roleId,
    status
  }
  return request({
    url: '/system/role/changeStatus',
    method: 'put',
    data: data
  })
}

// 删除角色
export function delRole(roleId) {
  return request({
    url: '/system/role/' + roleId,
    method: 'delete'
  })
}

// 导出角色
export function exportRole(query) {
  return request({
    url: '/system/role/export',
    method: 'get',
    params: query
  })
}

// 人员配置查询所有人员
export function getAllUser() {
  return request({
    url: '/system/role/getAllUser',
    method: 'get'
  })
}

// 查询角色下用户
export function getUserRole(roleId) {
  return request({
    url: '/system/role/getUserRole',
    method: 'get',
    params:{roleId}
  })
}

// 保存角色人员配置
export function saveRoleUser(data) {
  return request({
    url: '/system/role/saveRoleUser',
    method: 'post',
    data: data
  })
}


