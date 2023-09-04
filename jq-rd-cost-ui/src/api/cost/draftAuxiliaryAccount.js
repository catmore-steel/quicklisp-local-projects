import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询研发投入无形资产费用拟定;详细
export function getFee(id) {
  return request({
    url: '/cost/draftAuxiliaryAccount/' + id,
    method: 'get'
  })
}

// 新增研发投入无形资产费用拟定;
export function addFee(data) {
  return request({
    url: '/cost/draftAuxiliaryAccount',
    method: 'post',
    data: data
  })
}

// 修改研发投入无形资产费用拟定;
export function updateFee(data) {
  return request({
    url: '/cost/draftAuxiliaryAccount',
    method: 'put',
    data: data
  })
}

// 删除研发投入无形资产费用拟定;
export function delFee(id) {
  return request({
    url: '/cost/draftAuxiliaryAccount/' + id,
    method: 'delete'
  })
}

// 导出研发投入无形资产费用拟定;
export function exportFee(query) {
  return excelDownLoad('/cost/draftAuxiliaryAccount/export',query)
}




export function list(query) {
  return request({
    url: '/cost/draftAuxiliaryAccount/list',
    method: 'get',
    params: query
  })
}

export function getStatistics(query) {
  return request({
    url: '/cost/draftAuxiliaryAccount/statistics',
    method: 'get',
    params: query
  })
}


export function updateByBaseIpInfo(data) {
  return request({
    url: '/cost/budgetIpFee/updateByBaseIpInfo',
    method: 'post',
    data: data
  })
}