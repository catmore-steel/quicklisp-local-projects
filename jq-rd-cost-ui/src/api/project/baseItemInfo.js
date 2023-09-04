import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询项目管理;详细
export function getBaseItemInfo(id) {
  return request({
    url: '/project/baseItemInfo/' + id,
    method: 'get'
  })
}

// 新增项目管理;
export function addBaseItemInfo(data) {
  return request({
    url: '/project/baseItemInfo/',
    method: 'post',
    data: data
  })
}

// 修改项目管理;
export function updateBaseItemInfo(data) {
  return request({
    url: '/project/baseItemInfo/',
    method: 'put',
    data: data
  })
}

// 删除项目管理;
export function delBaseItemInfo(id) {
  return request({
    url: '/project/baseItemInfo/' + id,
    method: 'delete'
  })
}

// 导出项目管理;
export function exportBaseItemInfo(query) {
  return excelDownLoad('/project/baseItemInfo/export',query)
}

export function projectBaseItemInfoFindList(companyId) {
  return request({
    // url: '/project/baseItemInfo/findList',
    // method: 'post',
    // data: data||{}
    url: '/project/baseItemInfo/findList?companyId=' + companyId,
    method: 'get'
  })
}

export function selectItemByCompanyId(companyId) {
  return request({
    url: '/project/baseItemInfo/selectItemByCompanyId?companyId=' + companyId,
    method: 'get'
  })
}
