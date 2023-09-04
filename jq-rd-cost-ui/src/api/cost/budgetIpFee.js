import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询研发投入无形资产费用拟定;详细
export function getFee(id) {
  return request({
    url: '/cost/budgetIpFee/' + id,
    method: 'get'
  })
}

// 新增研发投入无形资产费用拟定;
export function addFee(data) {
  return request({
    url: '/cost/budgetIpFee',
    method: 'post',
    data: data
  })
}

// 修改研发投入无形资产费用拟定;
export function updateFee(data) {
  return request({
    url: '/cost/budgetIpFee',
    method: 'put',
    data: data
  })
}

// 删除研发投入无形资产费用拟定;
export function delFee(id) {
  return request({
    url: '/cost/budgetIpFee/' + id,
    method: 'delete'
  })
}

// 导出研发投入无形资产费用拟定;
export function exportFee(query) {
  return excelDownLoad('/cost/budgetIpFee/export',query)
}




export function findBaseIpInfoAll(query) {
  return request({
    url: '/cost/budgetIpFee/baseIpInfo/list',
    method: 'get',
    params: query
  })
}

export function getStatistics(query) {
  return request({
    url: '/cost/budgetIpFee/statistics',
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