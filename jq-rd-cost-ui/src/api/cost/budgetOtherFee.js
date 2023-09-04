import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询研发投入其他费用拟定;详细
export function getBudgetOtherFee(id) {
  return request({
    url: '/cost/budgetOtherFee/' + id,
    method: 'get'
  })
}

// 新增研发投入其他费用拟定;
export function addBudgetOtherFee(data) {
  return request({
    url: '/cost/budgetOtherFee',
    method: 'post',
    data: data
  })
}

// 修改研发投入其他费用拟定;
export function updateBudgetOtherFee(data) {
  return request({
    url: '/cost/budgetOtherFee',
    method: 'put',
    data: data
  })
}

// 删除研发投入其他费用拟定;
export function delBudgetOtherFee(id) {
  return request({
    url: '/cost/budgetOtherFee/' + id,
    method: 'delete'
  })
}

// 导出研发投入其他费用拟定;
export function exportBudgetOtherFee(query) {
  return excelDownLoad('/cost/budgetOtherFee/export',query)
}


// 查询所有列表
export function selectAll(query) {
  return request({
    url: '/cost/budgetOtherFee/selectAll',
    method: 'get',
    params: query
  })
}

// 修改研发投入其他费用拟定;
export function updateByDev(data) {
  return request({
    url: '/cost/budgetOtherFee/updateByDev',
    method: 'post',
    data: data
  })
}

export function getStatistics(query) {
  return request({
    url: '/cost/budgetOtherFee/statistics',
    method: 'get',
    params: query
  })
}
