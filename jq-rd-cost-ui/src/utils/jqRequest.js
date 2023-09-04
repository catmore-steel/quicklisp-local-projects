import request from '@/utils/request'
import {Loading} from "element-ui";

let loadingInstance = null
let num = 0
let target ;

function jqRequest(obj) {
  // console.log(obj)
  //过滤请求参数
  // url 请求地址
  // method 请求方式
  // params get参数拼接
  // data post参数填装
  let axios = {
    url: obj.url, method: obj.method, params: obj.params, data: obj.data,timeout: obj.timeout
  }
  //设置为true
  if (obj.loading) {
    if(!num || !target){
      let text = obj.loading.text
      target = document.querySelector(obj.loading.target)
      //处理方式
      loadingInstance = Loading.service({
        target: target, background: '#a6d6ff26', text: text
      })
      num ++
    }
  } else {
    if(!num || !target){
      let text = "加载中..."
      target = document.querySelector(".app-container") // 未获取到
      // console.log(target)
      if (target) {
        //处理方式
        loadingInstance = Loading.service({
          target: target, background: '#a6d6ff26', text: text
        })
      }
    }
    num ++
  }

  //多层拦截器处理  request 公共参数处理， 业务then 处理 ，转递最终结果并 关闭动画
  return request(axios).finally(res => {
    num --
    if(num == 0 && loadingInstance){
      loadingInstance.close()
    }
  })
}

export default jqRequest
