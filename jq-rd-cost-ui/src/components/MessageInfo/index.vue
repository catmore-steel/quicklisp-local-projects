<template>
  <el-popover
    placement="left-start"
    width="1000"
    trigger="click">
    <el-table :data="gridData">
      <el-table-column width="100" prop="sendUserName" label="发送人"></el-table-column>
      <el-table-column width="180" prop="sendTime" label="发送日期"></el-table-column>
      <el-table-column  label="消息内容">
        <template slot-scope="scope">
          <textarea rows="2" style="width: 100%;border-color:white">{{scope.row.content}}</textarea>
        </template>
      </el-table-column>
      <el-table-column width="80" label="类型">
        <template slot-scope="scope">
          <el-tooltip v-show="scope.row.warnType == '1'? true:false" class="item" effect="dark" content="提示"
                      placement="top">
            <i class="iconfont jq-ico-tips" style="color: #00afff"/>
          </el-tooltip>
          <el-tooltip v-show="scope.row.warnType == '2'? true:false" class="item" effect="dark" content="告警"
                      placement="top">
            <i class="iconfont jq-ico-tips" style="color: red"/>
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column width="180" label="操作" align="center" class-name="small-padding fixed-width">
        <template slot-scope="scope">
          <el-button
            type="text"
            icon="iconfont jq-ico-chuli"
            v-show="checkRouter(scope.row.router,1)"
            @click="joinDeal(scope.row.router)"
          >进入处理
          </el-button>
          <el-button v-show="checkRouter(scope.row.router,2)"

                     type="text">正在处理中</el-button>
          <el-checkbox v-model="scope.row.hasRead" style="color: #409EFF;font-size: 12px;"
                       @change="setHasRead(scope.row.id)">已读
          </el-checkbox>
        </template>
      </el-table-column>
    </el-table>
    <i slot="reference" :class="gridData.length==0 ? 'iconfont jq-ico-msg':'iconfont jq-ico-msg animImage'"
       icon-class="size" style="font-size: 23px;"
       @click="showMessageInfo"><span style="font-size: 10px" v-if="gridData.length > 0">{{gridData.length}}</span>
    </i>

  </el-popover>
</template>


<script>

  import {
    listMessageinfo,
    getMessageinfo,
    delMessageinfo,
    addMessageinfo,
    updateMessageinfo,
    exportMessageinfo
  } from "@/api/system/messageinfo";

  import path from 'path'
  import {isNullOrEmpty} from "../../utils/jq";


  export default {
    name: "MessageInfo",
    data() {
      return {
        gridData: [],
      }
    },
    created() {
      this.showMessageInfo();
    },
    methods: {
      showMessageInfo() {
        listMessageinfo({
          status: '1'
        }).then(response => {
          this.gridData = response.rows;
        });
      },
      joinDeal(router) {
        this.$router.push(router);
      },
      setHasRead(id) {
        updateMessageinfo({
          id: id,
          status: '2'
        }).then(response => {
          if (response.code == 200) {
            this.msgSuccess("消息已标记为已读");
            this.showMessageInfo();
          }
        });
      },
      checkRouter(router,type){
        if (isNullOrEmpty(router)){ //与当前路由一直
            return false;
        }
        let router_path = this.$route.path;
        if (type == 1){
          if (router_path == router) {  //与当前路由一一致
            return false;
          }else{
            return true;
          }
        }
        if (type == 2){
          if (router_path == router){
            return true;
          }else{
            return false;
          }
        }
      }
    }
  }
</script>

<style scoped>

  .el-table__row .el-tooltip {
    width: 0px;
  }

  .el-table__row .el-button--text {
    width: auto;
    margin-right: 20px;
    font-size: 14px;
  }


  .animImage {
    -webkit-animation-name: imageAnim;
    -webkit-animation-duration: 1s;
    -webkit-animation-iteration-count: infinite;
    -webkit-animation-direction: alternate;
    -webkit-animation-timing-function: ease;
    -webkit-animation-play-state: running;
  }

  @-webkit-keyframes imageAnim {
    0% {
      opacity: 1;
      color: #409EFF
    }
    50% {
      opacity: 0.5;
      color: #409EFF
    }
    100% {
      opacity: 0;
      color: #409EFF
    }
  }
</style>
