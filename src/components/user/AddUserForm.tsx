'use client';

import React, { useState } from 'react';
import { Button } from '@/components/ui/Button';
import { CreateUserSecurityRequest } from '@/types/user';
import { userService } from '@/services/userService';

interface AddUserFormProps {
  onUserAdded: (userId: string) => void;
  onExit: () => void;
}

export function AddUserForm({ onUserAdded, onExit }: AddUserFormProps) {
  const [formData, setFormData] = useState({
    firstName: '',
    lastName: '',
    userId: '',
    password: '',
    userType: ''
  });
  const [message, setMessage] = useState('');
  const [messageType, setMessageType] = useState<'success' | 'error' | 'info'>('info');
  const [loading, setLoading] = useState(false);
  const [cursorField, setCursorField] = useState<keyof typeof formData>('firstName');

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

  // Handle ENTER key press - validate and add user (COUSR01C business logic)
  const handleEnterKey = async () => {
    setLoading(true);
    setMessage('');

    try {
      // Validate all fields in sequence (COBOL business logic)
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

      const userIdValidation = userService.validateUserId(formData.userId);
      if (!userIdValidation.valid) {
        setMessage(userIdValidation.message);
        setMessageType('error');
        setCursorField('userId');
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

      // All validations passed, create user
      const createRequest: CreateUserSecurityRequest = {
        userId: formData.userId,
        password: formData.password,
        userType: formData.userType === 'A' ? 'ADMIN' : 'GENERAL',
        programName: 'COSGN00C',
        transactionId: 'CC00',
        active: true
      };

      const newUser = await userService.createUser(createRequest);
      
      // Success - clear form and show success message (COBOL business logic)
      setFormData({
        firstName: '',
        lastName: '',
        userId: '',
        password: '',
        userType: ''
      });
      setCursorField('firstName');
      setMessage(`User ${newUser.userId} has been added ...`);
      setMessageType('success');
      
      onUserAdded(newUser.userId);

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to Add User...';
      
      if (errorMessage.includes('already exists')) {
        setMessage('User ID already exist...');
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

  // Handle PF3 key press - exit
  const handlePF3Key = () => {
    onExit();
  };

  // Handle PF4 key press - clear screen
  const handlePF4Key = () => {
    setFormData({
      firstName: '',
      lastName: '',
      userId: '',
      password: '',
      userType: ''
    });
    setMessage('');
    setCursorField('firstName');
  };

  // Handle form submission
  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    handleEnterKey();
  };

  // Handle key press events (simulating COBOL function keys)
  const handleKeyPress = (e: React.KeyboardEvent) => {
    if (e.key === 'F3' || (e.altKey && e.key === '3')) {
      e.preventDefault();
      handlePF3Key();
    } else if (e.key === 'F4' || (e.altKey && e.key === '4')) {
      e.preventDefault();
      handlePF4Key();
    }
  };

  return (
    <div className="min-h-screen bg-gray-900 text-green-400 font-mono">
      {/* Header */}
      <div className="bg-gray-800 p-4 border-b border-green-400">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-xl font-bold">CARDDEMO</h1>
            <h2 className="text-lg">User Add</h2>
          </div>
          <div className="text-right">
            <div>TRAN: CU01</div>
            <div>PGM: COUSR01C</div>
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
              disabled={loading}
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
              disabled={loading}
            />
            <span className="text-gray-500">(Max 20 characters)</span>
          </div>

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
              disabled={loading}
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
              disabled={loading}
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
              {loading ? 'Adding User...' : 'ENTER - Add User'}
            </Button>
            
            <Button
              type="button"
              onClick={handlePF3Key}
              variant="secondary"
              disabled={loading}
              className="bg-gray-700 hover:bg-gray-600 text-white"
            >
              PF3 - Exit
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
              <li>Fill in all required fields</li>
              <li>Press ENTER to add the user</li>
              <li>Press PF3 to exit to Admin Menu</li>
              <li>Press PF4 to clear all fields</li>
              <li>User Type: A = Administrator, U = General User</li>
            </ul>
          </div>
        </form>
      </div>
    </div>
  );
}