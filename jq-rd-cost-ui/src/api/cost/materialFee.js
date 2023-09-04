import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询直接投入费用拟定;详细
export function getMaterialFee(id) {
  return request({
    url: '/cost/materialFee/' + id,
    method: 'get'
  })
}

// 新增直接投入费用拟定;
export function addMaterialFee(data) {
  return request({
    url: '/cost/materialFee',
    method: 'post',
    data: data
  })
}

// 修改直接投入费用拟定;
export function updateMaterialFee(data) {
  return request({
    url: '/cost/materialFee',
    method: 'put',
    data: data
  })
}

// 删除直接投入费用拟定;
export function delMaterialFee(id) {
  return request({
    url: '/cost/materialFee/' + id,
    method: 'delete'
  })
}

// 导出直接投入费用拟定;
export function exportMaterialFee(query) {
  return excelDownLoad('/cost/materialFee/export',query)
}
