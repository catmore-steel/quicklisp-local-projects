import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询原始序时账信息详细
export function getAccountInfo(id) {
  return request({
    url: '/cost/accountInfo/' + id,
    method: 'get'
  })
}

// 新增原始序时账信息
export function addAccountInfo(data) {
  return request({
    url: '/cost/accountInfo',
    method: 'post',
    data: data
  })
}

// 修改原始序时账信息
export function updateAccountInfo(data) {
  return request({
    url: '/cost/accountInfo',
    method: 'put',
    data: data
  })
}

// 删除原始序时账信息
export function delAccountInfo(id) {
  return request({
    url: '/cost/accountInfo/' + id,
    method: 'delete'
  })
}

// 导出原始序时账信息
export function exportAccountInfo(query) {
  return excelDownLoad('/cost/accountInfo/export', query)
}

// 查询总预算确认的序时账列表
export function getBaseAccountList(query) {
  return request({
    url: '/cost/accountInfo/getBaseAccountList',
    method: 'get',
    params: query
  })
}

// 查询序时帐分类占比饼图
export function getSequentialChartByCompanyIdAndItemNo(companyId, itemNo) {
  return request({
    url: '/cost/accountInfo/getSequentialChartByCompanyIdAndItemNo',
    method: 'get',
    params: { companyId: companyId, itemNo: itemNo }
  })
}

// 查询详情左表序时账记录
export function getListByCompanyIdAndItemNo(companyId, itemNo) {
  return request({
    url: '/cost/accountInfo/getListByCompanyIdAndItemNo',
    method: 'get',
    params: { companyId: companyId, itemNo: itemNo }
  })
}

// 序时账归类的保存
export function updateAccountType(data) {
  return request({
    url: '/cost/accountInfo/updateAccountType',
    method: 'put',
    data: data
  })
}

// 导出原始序时账信息
export function exportAccount(query) {
  return excelDownLoad('/cost/accountInfo/exportAccount', query)
}

