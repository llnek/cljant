/* Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Copyright © 2013-2024, Kenneth Leung. All rights reserved. */

package czlab.test.cljant;

import static org.junit.Assert.assertTrue;
import junit.framework.JUnit4TestAdapter;
import java.util.Map;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class JUnit {

  public static junit.framework.Test suite() {
    return new JUnit4TestAdapter(JUnit.class);
  }

  @BeforeClass
  public static void iniz() throws Exception {
  }

  @AfterClass
  public static void finz() {
  }

  @Before
  public void open() throws Exception {
  }

  @After
  public void close() throws Exception {
  }

  @Test
  public void testDummy() throws Exception {
    assertTrue(1 == 1);
  }


}


