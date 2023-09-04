<template>
  <div class="search_box ">
    <div class="search_box_header" @click="handleShowSearch">
      <div class="search_target">
        <span class="search_target_name">筛选项：</span>
        <el-tag
          class="search_target_item"
          v-for="(tag,index) in tags"
          :key="tag.name"
          closable
          @close="handleClose(index)"
          :type="tag.type">
          {{ tag.name }}
        </el-tag>
        <i v-if="tags.length > 0" class="el-icon-refresh header-refresh"  title="重置" @click.stop="resetForm"></i>
      </div>

      <i style="float: right;margin-right: 40px;line-height: 30px"
         :class="[showSearch?'is-open':'','el-icon-arrow-down','search-down']"></i>
    </div>
    <div class="line"></div>
    <el-collapse-transition>
      <div v-show="showSearch" :class="['search_box_content',lockStatus?'lock':'unlock']">
        <div class="search_box_form">
          <slot></slot>
        </div>
        <div class="search_box_operate">
          <el-button type="primary" icon="el-icon-search"  @click="queryForm">搜索</el-button>
          <el-button icon="el-icon-refresh"  @click="resetForm">重置</el-button>
          <el-button type="success" icon="el-icon-lock"  @click="lock">{{ lockStatus ? '解锁' : '锁定' }}
          </el-button>
        </div>
      </div>
    </el-collapse-transition>
  </div>
</template>

<script>
export default {
  name: 'JqSearch',
  data() {
    return {
      showSearch: false,  //影藏状态
      lockStatus: false,  // 解锁状态
      tags: []
    }
  },
  props: {
    formData: {
      type: Object
    },
    queryFormRefData:{
      type: Object
    }
  },
  methods: {
    // 点击tag的 x
    handleClose(index){
      // 重新组装一个 queryFrom 并更新 父组件
      let objKey = this.tags[index].valKey
      let tmpFormObj = {...this.formData}
      tmpFormObj[objKey] = null
      this.$emit('update:formData',tmpFormObj)
      this.$emit('query')
      this.tags.splice(index,1)
    },
    loadTags(){
      // 拿到的 Form 遍历 出所有 el-form-item
      this.queryFormRefData.$children.forEach(itemCmp => {
        itemCmp.$children.forEach(cmp2 => {
          cmp2.$children.forEach(cmp3 => {
            // cmp3.label | cmp3.prop | cmp3.fieldValue => '内部编号' 'caseNo' '01010'
            // 值为空的时候检查 tags里面是否有 ， 删掉
            if(this.isNullOrEmpty(cmp3.fieldValue)) {
              let index = this.tags.findIndex(item => item.name == cmp3.label)
              if(index !== -1) this.tags.splice(index,1)
              return
            }

            if(this.tags.length == 0){
              this.tags.push({name:cmp3.label,type: '',valKey:cmp3.prop})
            }else{
              // 如果当前 tags 不存在 搜索条件 就添加
              let index = this.tags.find(item => item.name == cmp3.label)
              if(!index)  this.tags.push({name:cmp3.label,type: '',valKey:cmp3.prop})
            }
          })
        })
      })
    },
    handleShowSearch() {
      if (this.lockStatus) {
        this.msgError("请先解锁搜索栏！")
        return
      }
      this.showSearch = !this.showSearch
      if (this.showSearch == true) {
        //收起操作 同时解锁
        this.lockStatus = false
      }
    },
    lock() {
      this.lockStatus = !this.lockStatus
    },
    queryForm() {
      this.loadTags()
      //如果当前页面浮动状态
      if(this.lockStatus == false){
        this.showSearch = false
      }
      this.$emit('query')
    },
    resetForm() {
      this.$emit('update:formData',{radioSearch:0})
      this.$emit('reset') // 为了兼容清除一些不在queryParams 里面的数据
      this.queryForm()
      this.tags = []
    }
  },
}
</script>

<style lang="scss" scoped>
.search_box {
  width: calc(100% + 40px);
  position: relative;
  top: -20px;
  margin-left: -20px;


  .search_box_header {
    z-index: 1;
    width: 100%;
    height: 30px;
    padding-left: 0px;
    line-height: 30px;


    .search-down {
      -webkit-transition: -webkit-transform .3s;
      transition: -webkit-transform .3s;
      transition: transform .3s;
      transition: transform .3s, -webkit-transform .3s;
    }

    .is-open {
      -webkit-transform: rotateZ(180deg);
      transform: rotateZ(180deg);
    }

    .search_target {
      width: 60%;
      float: left;
      margin-left: 20px;
      height: 28px;
      line-height: 28px;
    }

    .search_target_name {
      font-size: 14px;
      color: #606266;

    }

    .header-refresh{
      margin-left: 10px;
    }

    .header-refresh:hover{
      color: red;
    }
  }

  .line {
    background-color: #bbd1ad;
    height: 2px;
    box-shadow: 0 3px 10px #bbd1ad;
    -moz-box-shadow: 0 3px 8px #bbd1ad;
  }


  .search_box_content {
    width: 100%;
    transition: all 0.6s;
    top: 0;
    left: 0;
    z-index: 4;
    margin-right: 20px;
    align-items: center;


    .search_box_form {
      width: 100%;
      margin-top: 20px;
    }

    .search_box_operate {
      background: #eef1f6;
      padding: 5px;
      text-align: center;
      align-content: center;

      /deep/ .el-button {
        margin-left: 10px !important;
      }
    }
  }

  .unlock {
    position: absolute;
    z-index: 9;
    background: white;
  }

  .lock {
    position: relative;
    margin-top: 0px;
    background: white;
  }

}


</style>
