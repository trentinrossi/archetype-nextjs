'use client';

import React, { useState, useEffect } from 'react';
import { Button } from '@/components/ui/Button';
import { UserSecurityDTO, UpdateUserSecurityRequest } from '@/types/user';
import { userService } from '@/services/userService';

interface UpdateUserFormProps {
  selectedUserId?: string;
  onUserUpdated: (userId: string) => void;
  onExit: () => void;
}

export function UpdateUserForm({ selectedUserId, onUserUpdated, onExit }: UpdateUserFormProps) {
  const [formData, setFormData] = useState({
    userId: selectedUserId || '',
    firstName: '',
    lastName: '',
    password: '',
    userType: ''
  });
  const [originalData, setOriginalData] = useState<UserSecurityDTO | null>(null);
  const [message, setMessage] = useState('');
  const [messageType, setMessageType] = useState<'success' | 'error' | 'info'>('info');
  const [loading, setLoading] = useState(false);
  const [cursorField, setCursorField] = useState<keyof typeof formData>('userId');
  const [userLoaded, setUserLoaded] = useState(false);

  // Load user data if selectedUserId is provided
  useEffect(() => {
    if (selectedUserId && !userLoaded) {
      handleEnterKey();
    }
  }, [selectedUserId, userLoaded]);

  // Handle input changes with COBOL field length validation
  const handleInputChange = (field: keyof typeof formData, value: string) => {
    let processedValue = value;
    
    if (field === 'userId') {
      // Convert to uppercase and limit to 8 characters (COBOL business logic)
      processedValue = value.toUpperCase().slice(0, 8);
    } else if (field === 'password') {
      // Limit to 8 characters (COBOL business logic)
      processedValue = value.slice(0, 8);
    } else if (field === 'firstName' || field === 'lastName') {
      // Limit to 20 characters (COBOL business logic)
      processedValue = value.slice(0, 20);
    } else if (field === 'userType') {
      // Convert to uppercase and limit to 1 character (COBOL business logic)
      processedValue = value.toUpperCase().slice(0, 1);
    }

    setFormData(prev => ({
      ...prev,
      [field]: processedValue
    }));

    // Clear message when user starts typing
    if (message) {
      setMessage('');
    }
  };

  // Handle ENTER key press - lookup user (COUSR02C business logic)
  const handleEnterKey = async () => {
    if (!formData.userId.trim()) {
      setMessage('User ID can NOT be empty...');
      setMessageType('error');
      setCursorField('userId');
      return;
    }

    setLoading(true);
    setMessage('');

    try {
      const user = await userService.getUserById(formData.userId);
      
      // Populate form with user data
      setFormData({
        userId: user.userId,
        firstName: user.userType === 'ADMIN' ? 'System' : 'General',
        lastName: user.userType === 'ADMIN' ? 'Administrator' : 'User',
        password: user.password,
        userType: user.userType === 'ADMIN' ? 'A' : 'U'
      });
      
      setOriginalData(user);
      setUserLoaded(true);
      setMessage('Press PF5 key to save your updates ...');
      setMessageType('info');
      setCursorField('firstName');

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to lookup User...';
      
      if (errorMessage.includes('not found')) {
        setMessage('User ID NOT found...');
        setCursorField('userId');
      } else {
        setMessage(errorMessage);
        setCursorField('firstName');
      }
      setMessageType('error');
      
      // Clear form data except userId
      setFormData(prev => ({
        ...prev,
        firstName: '',
        lastName: '',
        password: '',
        userType: ''
      }));
      setOriginalData(null);
      setUserLoaded(false);
    } finally {
      setLoading(false);
    }
  };

  // Handle PF5 key press - update user (COUSR02C business logic)
  const handlePF5Key = async () => {
    if (!originalData) {
      setMessage('Please lookup a user first');
      setMessageType('error');
      return;
    }

    setLoading(true);
    setMessage('');

    try {
      // Validate all fields in sequence (COBOL business logic)
      const userIdValidation = userService.validateUserId(formData.userId);
      if (!userIdValidation.valid) {
        setMessage(userIdValidation.message);
        setMessageType('error');
        setCursorField('userId');
        return;
      }

      const firstNameValidation = userService.validateFirstName(formData.firstName);
      if (!firstNameValidation.valid) {
        setMessage(firstNameValidation.message);
        setMessageType('error');
        setCursorField('firstName');
        return;
      }

      const lastNameValidation = userService.validateLastName(formData.lastName);
      if (!lastNameValidation.valid) {
        setMessage(lastNameValidation.message);
        setMessageType('error');
        setCursorField('lastName');
        return;
      }

      const passwordValidation = userService.validatePassword(formData.password);
      if (!passwordValidation.valid) {
        setMessage(passwordValidation.message);
        setMessageType('error');
        setCursorField('password');
        return;
      }

      const userTypeValidation = userService.validateUserType(formData.userType);
      if (!userTypeValidation.valid) {
        setMessage(userTypeValidation.message);
        setMessageType('error');
        setCursorField('userType');
        return;
      }

      // Check if any changes were made (COBOL business logic)
      const hasChanges = 
        formData.password !== originalData.password ||
        (formData.userType === 'A' ? 'ADMIN' : 'GENERAL') !== originalData.userType;

      if (!hasChanges) {
        setMessage('Please modify to update ...');
        setMessageType('error');
        return;
      }

      // Update user
      const updateRequest: UpdateUserSecurityRequest = {
        password: formData.password,
        userType: formData.userType === 'A' ? 'ADMIN' : 'GENERAL'
      };

      const updatedUser = await userService.updateUser(formData.userId, updateRequest);
      
      setMessage(`User ${updatedUser.userId} has been updated ...`);
      setMessageType('success');
      setOriginalData(updatedUser);
      
      onUserUpdated(updatedUser.userId);

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to Update User...';
      
      if (errorMessage.includes('not found')) {
        setMessage('User ID NOT found...');
        setCursorField('userId');
      } else {
        setMessage(errorMessage);
        setCursorField('firstName');
      }
      setMessageType('error');
    } finally {
      setLoading(false);
    }
  };

  // Handle PF3 key press - save and exit
  const handlePF3Key = async () => {
    if (originalData && userLoaded) {
      await handlePF5Key();
    }
    onExit();
  };

  // Handle PF4 key press - clear screen
  const handlePF4Key = () => {
    setFormData({
      userId: '',
      firstName: '',
      lastName: '',
      password: '',
      userType: ''
    });
    setOriginalData(null);
    setUserLoaded(false);
    setMessage('');
    setCursorField('userId');
  };

  // Handle PF12 key press - cancel
  const handlePF12Key = () => {
    onExit();
  };

  // Handle form submission
  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (userLoaded) {
      handlePF5Key();
    } else {
      handleEnterKey();
    }
  };

  // Handle key press events (simulating COBOL function keys)
  const handleKeyPress = (e: React.KeyboardEvent) => {
    if (e.key === 'F3' || (e.altKey && e.key === '3')) {
      e.preventDefault();
      handlePF3Key();
    } else if (e.key === 'F4' || (e.altKey && e.key === '4')) {
      e.preventDefault();
      handlePF4Key();
    } else if (e.key === 'F5' || (e.altKey && e.key === '5')) {
      e.preventDefault();
      handlePF5Key();
    } else if (e.key === 'F12' || (e.altKey && e.key === '12')) {
      e.preventDefault();
      handlePF12Key();
    }
  };

  return (
    <div className="min-h-screen bg-gray-900 text-green-400 font-mono">
      {/* Header */}
      <div className="bg-gray-800 p-4 border-b border-green-400">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-xl font-bold">CARDDEMO</h1>
            <h2 className="text-lg">User Update</h2>
          </div>
          <div className="text-right">
            <div>TRAN: CU02</div>
            <div>PGM: COUSR02C</div>
            <div>{new Date().toLocaleDateString('en-US', { 
              month: '2-digit', 
              day: '2-digit', 
              year: '2-digit' 
            })}</div>
            <div>{new Date().toLocaleTimeString('en-US', { 
              hour12: false,
              hour: '2-digit',
              minute: '2-digit',
              second: '2-digit'
            })}</div>
          </div>
        </div>
      </div>

      {/* Main form area */}
      <div className="p-8">
        <form onSubmit={handleSubmit} onKeyDown={handleKeyPress} className="space-y-6">
          {/* User ID field */}
          <div className="flex items-center space-x-4">
            <label htmlFor="userId" className="w-32 text-right">
              User ID:
            </label>
            <input
              id="userId"
              type="text"
              value={formData.userId}
              onChange={(e) => handleInputChange('userId', e.target.value)}
              className={`bg-gray-800 border ${
                cursorField === 'userId' ? 'border-yellow-400' : 'border-green-400'
              } text-green-400 px-2 py-1 w-32 focus:outline-none focus:border-yellow-400`}
              maxLength={8}
              autoFocus={cursorField === 'userId'}
              disabled={loading}
            />
            <span className="text-gray-500">(Max 8 characters)</span>
          </div>

          {/* First Name field */}
          <div className="flex items-center space-x-4">
            <label htmlFor="firstName" className="w-32 text-right">
              First Name:
            </label>
            <input
              id="firstName"
              type="text"
              value={formData.firstName}
              onChange={(e) => handleInputChange('firstName', e.target.value)}
              className={`bg-gray-800 border ${
                cursorField === 'firstName' ? 'border-yellow-400' : 'border-green-400'
              } text-green-400 px-2 py-1 w-64 focus:outline-none focus:border-yellow-400`}
              maxLength={20}
              autoFocus={cursorField === 'firstName'}
              disabled={loading || !userLoaded}
            />
            <span className="text-gray-500">(Max 20 characters)</span>
          </div>

          {/* Last Name field */}
          <div className="flex items-center space-x-4">
            <label htmlFor="lastName" className="w-32 text-right">
              Last Name:
            </label>
            <input
              id="lastName"
              type="text"
              value={formData.lastName}
              onChange={(e) => handleInputChange('lastName', e.target.value)}
              className={`bg-gray-800 border ${
                cursorField === 'lastName' ? 'border-yellow-400' : 'border-green-400'
              } text-green-400 px-2 py-1 w-64 focus:outline-none focus:border-yellow-400`}
              maxLength={20}
              autoFocus={cursorField === 'lastName'}
              disabled={loading || !userLoaded}
            />
            <span className="text-gray-500">(Max 20 characters)</span>
          </div>

          {/* Password field */}
          <div className="flex items-center space-x-4">
            <label htmlFor="password" className="w-32 text-right">
              Password:
            </label>
            <input
              id="password"
              type="password"
              value={formData.password}
              onChange={(e) => handleInputChange('password', e.target.value)}
              className={`bg-gray-800 border ${
                cursorField === 'password' ? 'border-yellow-400' : 'border-green-400'
              } text-green-400 px-2 py-1 w-32 focus:outline-none focus:border-yellow-400`}
              maxLength={8}
              autoFocus={cursorField === 'password'}
              disabled={loading || !userLoaded}
            />
            <span className="text-gray-500">(Max 8 characters)</span>
          </div>

          {/* User Type field */}
          <div className="flex items-center space-x-4">
            <label htmlFor="userType" className="w-32 text-right">
              User Type:
            </label>
            <input
              id="userType"
              type="text"
              value={formData.userType}
              onChange={(e) => handleInputChange('userType', e.target.value)}
              className={`bg-gray-800 border ${
                cursorField === 'userType' ? 'border-yellow-400' : 'border-green-400'
              } text-green-400 px-2 py-1 w-12 focus:outline-none focus:border-yellow-400`}
              maxLength={1}
              autoFocus={cursorField === 'userType'}
              disabled={loading || !userLoaded}
            />
            <span className="text-gray-500">(A=Admin, U=User)</span>
          </div>

          {/* Action buttons */}
          <div className="flex space-x-4 mt-8">
            <Button
              type="submit"
              disabled={loading}
              className="bg-green-700 hover:bg-green-600 text-white"
            >
              {loading ? 'Processing...' : userLoaded ? 'ENTER - Update' : 'ENTER - Lookup'}
            </Button>
            
            <Button
              type="button"
              onClick={handlePF3Key}
              variant="secondary"
              disabled={loading}
              className="bg-gray-700 hover:bg-gray-600 text-white"
            >
              PF3 - Save & Exit
            </Button>
            
            <Button
              type="button"
              onClick={handlePF4Key}
              variant="outline"
              disabled={loading}
              className="bg-gray-700 hover:bg-gray-600 text-white border-gray-500"
            >
              PF4 - Clear
            </Button>
            
            <Button
              type="button"
              onClick={handlePF5Key}
              disabled={loading || !userLoaded}
              className="bg-blue-700 hover:bg-blue-600 text-white disabled:opacity-50"
            >
              PF5 - Update
            </Button>
            
            <Button
              type="button"
              onClick={handlePF12Key}
              variant="destructive"
              disabled={loading}
              className="bg-red-700 hover:bg-red-600 text-white"
            >
              PF12 - Cancel
            </Button>
          </div>

          {/* Message area */}
          {message && (
            <div className={`mt-6 p-3 border ${
              messageType === 'success' ? 'border-green-400 text-green-400' :
              messageType === 'error' ? 'border-red-400 text-red-400' :
              'border-yellow-400 text-yellow-400'
            } bg-gray-800`}>
              {message}
            </div>
          )}

          {/* Instructions */}
          <div className="mt-8 text-gray-500 text-sm">
            <p>Instructions:</p>
            <ul className="list-disc list-inside mt-2 space-y-1">
              <li>Enter User ID and press ENTER to lookup user</li>
              <li>Modify fields as needed, then press PF5 to update</li>
              <li>Press PF3 to save changes and exit</li>
              <li>Press PF4 to clear all fields</li>
              <li>Press PF12 to cancel without saving</li>
            </ul>
          </div>
        </form>
      </div>
    </div>
  );
}