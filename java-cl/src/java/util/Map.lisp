(in-package #:java.util)

(defclass JMap ()
  ())

(defmethod size ((jmap JMap))
  ())

(defmethod isEmpty ((jmap JMap))
  ())

(defmethod containsKey ((jmap JMap))
  ())

(defmethod containsValue ((jmap JMap))
  ())

(defmethod get ((jmap JMap))
  ())

(defmethod put ((jmap JMap))
  ())

(defmethod remove ((jmap JMap))
  ())

(defmethod putAll ((jmap JMap))
  ())

(defmethod clear ((jmap JMap))
  ())

(defmethod keySet ((jmap JMap))
  ())

(defmethod values ((jmap JMap))
  ())

(defmethod entrySet ((jmap JMap))
  ())

(defmethod equals ((jmap JMap))
  ())

(defmethod hashCode ((jmap JMap))
  ())

(defmethod getOrDefault ((jmap JMap))
  ())


(defmethod forEach ((jmap JMap) (action BiConsumer))
  ())

(defmethod replaceAll ((jmap JMap) (action BiFunction))
  ())

(defmethod putIfAbsent ((jmap JMap) key value)
  ())

(defmethod remove ((jmap JMap) key value)
  ())

(defmethod replace ((jmap JMap) key oldValue newValue)
  ())

(defmethod replace ((jmap JMap) key value)
  ())

(defmethod computeIfAbsent ((jmap JMap) key (mappingFunction JFunction))
  ())

(defmethod computeIfPresent ((jmap JMap) (remappingFunction BiFunction))
  ())

(defmethod compute ((jmap JMap) (remappingFunction BiFunction))
  ())

(defmethod merge ((jmap JMap) key value (remappingFunction BiFunction))
  ())




;;;;;;;;  Entry  ;;;;;;;;

(defclass Entry ()
  ())


(defmethod getKey ((entry Entry))
  ())

(defmethod getValue ((entry Entry))
  ())

(defmethod setValue ((entry Entry))
  ())

(defmethod equals ((entry Entry))
  ())

(defmethod hashCode ((entry Entry))
  ())

(defmethod comparingByKey ((entry Entry))
  ())

(defmethod comparingByValue ((entry Entry))
  ())

(defmethod comparingByKey ((entry Entry) (comparator Comparator))
  ())

(defmethod comparingByValue ((entry Entry) (comparator Comparator))
  ())

