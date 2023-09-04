import request from '@/utils/jqRequest'
import excelDownLoad from '@/utils/exceldownload'

export function getItemListByCompany(companyId) {
  return request({
    url: '/project/itemInfo/listItemByCompany?companyId=' + companyId,
    method: 'get'
  })
}

// 查询项目管理详细
export function getItemInfo(id) {
  return request({
    url: '/project/itemInfo/' + id,
    method: 'get'
  })
}

export function getItemInfoByNo(itemNo) {
  return request({
    url: '/project/itemInfo/getItemInfoByNo?itemNo=' + itemNo,
    method: 'get'
  })
}

export function getAnalysisResult(itemNo) {
  return request({
    url: '/project/itemInfo/getAnalysisResult/' + itemNo,
    method: 'get'
  })
}

// 新增项目管理
export function addItemInfo(data) {
  return request({
    url: '/project/itemInfo',
    method: 'post',
    data: data
  })
}

// 修改项目管理
export function updateItemInfo(data) {
  return request({
    url: '/project/itemInfo',
    method: 'put',
    data: data
  })
}

// 删除项目管理
export function delItemInfo(id) {
  return request({
    url: '/project/itemInfo/' + id,
    method: 'delete'
  })
}

// 导出项目管理
export function exportItemInfo(query) {
  return excelDownLoad('/project/itemInfo/export',query)
}

export function analysisItemFile(itemNo) {
  return request({
    url: '/project/itemInfo/analysisItemFile/' + itemNo,
    method: 'post'
  })
}

export function exportFolder(itemNo) {
  return request({
    url: '/project/itemInfo/exportFolder?itemNo=' + itemNo,
    method: 'get',
  })
}

export function getDataMap(itemNo) {
  return request({
    url: '/project/itemInfo/getDataMap?itemNo=' + itemNo,
    method: 'get',
  })
}

export function listSubResultByItemNo(itemNo) {
  return request({
    url: '/project/itemInfo/listSubResultByItemNo?itemNo=' + itemNo,
    method: 'get',
  })
}

