import request from '@/utils/jqRequest'
import {praseStrEmpty} from "@/utils/jq";

// 查询用户列表
export function listUser(query) {
  return request({
    url: '/system/user/list',
    method: 'get',
    params: query
  })
}

// 下拉 选择提案人树
export function selectDeptUser(query) {
  return request({
    url: '/system/user/selectDeptUser',
    method: 'get',
    params: query
  })
}

// 查询代理机构用户列表
export function listAgentUser(query) {
  return request({
    url: '/system/user/listAgent',
    method: 'get',
    params: query
  })
}

// 查询当前登录人所在代理机构的代理人列表
export function listAgentCompanyUser() {
  return request({
    url: '/system/user/listAgentCompanyUser',
    method: 'get'
  })
}

// 查询对应代理机构用户列表
export function listAgentUserBox(agentDeptId) {
  return request({
    url: '/system/user/listAgentBox/' + agentDeptId,
    method: 'get',
  })
}

// 查询用户详细
export function getUser(userId) {
  return request({
    url: '/system/user/' + praseStrEmpty(userId),
    method: 'get'
  })
}

// 新增用户
export function addUser(data) {
  return request({
    url: '/system/user',
    method: 'post',
    data: data
  })
}

// 新增代理机构用户
export function addAgentUser(data) {
  return request({
    url: '/system/user/agent',
    method: 'post',
    data: data
  })
}

// 修改用户
export function updateUser(data) {
  return request({
    url: '/system/user',
    method: 'put',
    data: data
  })
}

// 修改代理机构用户
export function updateAgentUser(data) {
  return request({
    url: '/system/user/agent',
    method: 'put',
    data: data
  })
}

// 删除用户
export function delUser(userId) {
  return request({
    url: '/system/user/' + userId,
    method: 'delete'
  })
}

// 导出用户
export function exportUser(query) {
  return request({
    url: '/system/user/export',
    method: 'get',
    params: query
  })
}

// 用户密码重置
export function resetUserPwd(userId, password) {
  const data = {
    userId,
    password
  }
  return request({
    url: '/system/user/resetPwd',
    method: 'put',
    data: data
  })
}

// 代理机构用户密码重置
export function resetUserPass(userId) {
  return request({
    url: '/system/user/resetUserPass/' + userId,
    method: 'put',
  })
}

// 用户状态修改
export function changeUserStatus(userId, status) {
  const data = {
    userId,
    status
  }
  return request({
    url: '/system/user/changeStatus',
    method: 'put',
    data: data
  })
}

// 查询用户个人信息
export function getUserProfile() {
  return request({
    url: '/system/user/profile',
    method: 'get'
  })
}

// 修改用户个人信息
export function updateUserProfile(data) {
  return request({
    url: '/system/user/profile',
    method: 'put',
    data: data
  })
}

// 用户密码重置
export function updateUserPwd(oldPassword, newPassword) {
  const data = {
    oldPassword,
    newPassword
  }
  return request({
    url: '/system/user/profile/updatePwd',
    method: 'put',
    params: data
  })
}

// 用户头像上传
export function uploadAvatar(data) {
  return request({
    url: '/system/user/profile/avatar',
    method: 'post',
    data: data
  })
}

// 下载用户导入模板
export function importTemplate() {
  return request({
    url: '/system/user/importTemplate',
    method: 'get'
  })
}


// 查询代理机构代理人列表
export function getAgentUser() {
  return request({
    url: '/system/user/getAgentUser',
    method: 'get'
  })
}

// 查询代理机构递交代理人数据
export function getSendAgentUser(){
  return request({
    url: '/system/user/getSendAgentUser',
    method: 'get'
  })
}
