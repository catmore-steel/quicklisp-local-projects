import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询直接投入费用拟定;详细
export function getFee(id) {
  return request({
    url: '/cost/budgetMaterialFee/' + id,
    method: 'get'
  })
}

// 新增直接投入费用拟定;
export function addFee(data) {
  return request({
    url: '/cost/budgetMaterialFee',
    method: 'post',
    data: data
  })
}

// 修改直接投入费用拟定;
export function updateFee(data) {
  return request({
    url: '/cost/budgetMaterialFee',
    method: 'put',
    data: data
  })
}

// 删除直接投入费用拟定;
export function delFee(id) {
  return request({
    url: '/cost/budgetMaterialFee/' + id,
    method: 'delete'
  })
}

// 导出直接投入费用拟定;
export function exportFee(query) {
  return excelDownLoad('/cost/budgetMaterialFee/export',query)
}




export function findGroupingByAccDateYm(query) {
  return request({
    url: '/cost/budgetMaterialFee/findGroupingByAccDateYm',
    method: 'get',
    params: query
  })
}

export function listAll(query) {
  return request({
    url: '/cost/budgetMaterialFee/listAll',
    method: 'get',
    params: query
  })
}

export function saveOrUpdateByBudgetMaterialFeeForm(data) {
  return request({
    url: '/cost/budgetMaterialFee/saveOrUpdateByBudgetMaterialFeeForm',
    method: 'post',
    data: data
  })
}

export function getStatistics(query) {
  return request({
    url: '/cost/budgetMaterialFee/statistics',
    method: 'get',
    params: query
  })
}