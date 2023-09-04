import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询企业人员工资记录;详细
export function getUserSalary(id) {
  return request({
    url: '/cost/userSalary/' + id,
    method: 'get'
  })
}

// 新增企业人员工资记录;
export function addUserSalary(data) {
  return request({
    url: '/cost/userSalary',
    method: 'post',
    data: data
  })
}

// 修改企业人员工资记录;
export function updateUserSalary(data) {
  return request({
    url: '/cost/userSalary',
    method: 'put',
    data: data
  })
}

// 删除企业人员工资记录;
export function delUserSalary(id) {
  return request({
    url: '/cost/userSalary/delete/' + id,
    method: 'delete'
  })
}

// 导出企业人员工资记录;
export function exportUserSalary(query) {
  return excelDownLoad('/cost/userSalary/export',query)
}
