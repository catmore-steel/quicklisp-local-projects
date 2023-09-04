import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询企业人员信息详细
export function getUserInfo(userInfoId) {
  return request({
    url: '/cost/userInfo/getUserInfoById',
    method: 'get',
    params: { userInfoId: userInfoId }
  })
}

// 新增企业人员信息
export function addUserInfo(data) {
  return request({
    url: '/cost/userInfo/save',
    method: 'post',
    data: data
  })
}

// 修改企业人员信息
export function updateUserInfo(data) {
  return request({
    url: '/cost/userInfo/update',
    method: 'put',
    data: data
  })
}

// 删除企业人员信息
export function delUserInfo(id) {
  return request({
    url: '/cost/userInfo/delete/' + id,
    method: 'delete'
  })
}

// 导出企业人员信息
export function exportUserInfo(query) {
  return excelDownLoad('/cost/userInfo/export', query)
}

// 查询使用人信息
export function getUserNameInfo(companyId, itemNo) {
  return request({
    url: '/cost/userInfo/getUserNameByCompanyIdAndItemNo',
    method: 'get',
    params: { companyId: companyId, itemNo: itemNo }
  })
}
