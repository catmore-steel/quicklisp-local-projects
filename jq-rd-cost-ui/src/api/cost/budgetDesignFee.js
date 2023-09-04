import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询研发投入新品设计拟定详细
export function getBudgetDesignFee(id) {
  return request({
    url: '/cost/budgetDesignFee/' + id,
    method: 'get'
  })
}

// 新增研发投入新品设计拟定
export function addBudgetDesignFee(data) {
  return request({
    url: '/cost/budgetDesignFee',
    method: 'post',
    data: data
  })
}

// 修改研发投入新品设计拟定
export function updateBudgetDesignFee(data) {
  return request({
    url: '/cost/budgetDesignFee',
    method: 'put',
    data: data
  })
}

// 删除研发投入新品设计拟定
export function delBudgetDesignFee(id) {
  return request({
    url: '/cost/budgetDesignFee/' + id,
    method: 'delete'
  })
}

// 导出研发投入新品设计拟定
export function exportBudgetDesignFee(query) {
  return excelDownLoad('/cost/budgetDesignFee/export',query)
}

// 查询所有列表
export function selectDes(query) {
  return request({
    url: '/cost/budgetDesignFee/selectDes',
    method: 'get',
    params: query
  })
}

// 修改研发投入其他费用拟定;
export function updateByDes(data) {
  return request({
    url: '/cost/budgetDesignFee/updateByDes',
    method: 'post',
    data: data
  })
}

export function getStatistics(query) {
  return request({
    url: '/cost/budgetDesignFee/statistics',
    method: 'get',
    params: query
  })
}
