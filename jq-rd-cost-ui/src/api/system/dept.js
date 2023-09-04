import request from '@/utils/jqRequest'

// 查询申报机构列表
export function listDept(query) {
  return request({
    url: '/system/dept/list',
    method: 'get',
    params: query
  })
}

// 查询代理机构机构列表，和下述列表区分，此列表为了下拉显示
export function listAgentDepts() {
  return request({
    url: '/system/dept/listAgentDept',
    method: 'get'
  })
}

// 查询代理部门列表
export function listAgentDept(query) {
  return request({
    url: '/system/dept/listAgent',
    method: 'get',
    params: query
  })
}

// 查询部门列表（排除节点）
export function listDeptExcludeChild(deptId) {
  return request({
    url: '/system/dept/list/exclude/' + deptId,
    method: 'get'
  })
}

// 查询部门详细
export function getDept(deptId) {
  return request({
    url: '/system/dept/' + deptId,
    method: 'get'
  })
}

// 重置密码
export function resetPass(deptId) {
  return request({
    url: '/system/dept/agent/reset/' + deptId,
    method: 'get'
  })
}

//查询代理机构详细
export function getAgentDept(deptId) {
  return request({
    url: '/system/dept/agent/' + deptId,
    method: 'get'
  })
}

// 查询部门下拉树结构
export function treeselect() {
  return request({
    url: '/system/dept/treeselect',
    method: 'get'
  })
}

// 根据角色ID查询部门树结构
export function roleDeptTreeselect(roleId) {
  return request({
    url: '/system/dept/roleDeptTreeselect/' + roleId,
    method: 'get'
  })
}

// 新增部门
export function addDept(data) {
  return request({
    url: '/system/dept',
    method: 'post',
    data: data
  })
}

// 新增代理机构部门
export function addAgentDept(data) {
  return request({
    url: '/system/dept/agent',
    method: 'post',
    data: data
  })
}

// 修改部门
export function updateDept(data) {
  return request({
    url: '/system/dept',
    method: 'put',
    data: data
  })
}

// 修改代理机构部门
export function updateAgentDept(data) {
  return request({
    url: '/system/dept/agent',
    method: 'put',
    data: data
  })
}

// 删除部门
export function delDept(deptIds) {
  return request({
    url: '/system/dept/' + deptIds,
    method: 'delete'
  })
}

// 根据专利权人deptId查询申请人地区、发明人相关信息
export function getPatenteeInfo(deptId) {
  return request({
    url: '/system/dept/getPatenteeInfo/' + deptId,
    method: 'get'
  })
}

// 查询所有的申报机构列表（不受角色权限控制）
export function listAllApplyDept(query) {
  return request({
    url: '/system/dept/listAllApplyDept',
    method: 'get',
    params: query
  })
}

// 查询是申请人的申报机构列表（不受角色权限控制）
export function listPatenteeDept() {
  return request({
    url: '/system/dept/listPatentee',
    method: 'get'
  })
}

// 获取用户一级部门信息
export function getFirstLevelDeptByUserId(userId) {
  return request({
    url: '/system/dept/getFirstLevelDeptByUserId?userId=' + userId,
    method: 'get'
  })
}

