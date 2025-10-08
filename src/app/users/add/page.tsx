'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input, Select } from '@/components/ui';
import { userService } from '@/services/userService';
import {
  UserCreateRequest,
  UserFormData,
  UserFormErrors,
  USER_TYPE_OPTIONS
} from '@/types/user';

interface AddUserPageState {
  formData: UserFormData;
  errors: UserFormErrors;
  isLoading: boolean;
  message: string;
  messageType: 'success' | 'error' | 'info' | '';
}

const INITIAL_FORM_DATA: UserFormData = {
  userId: '',
  firstName: '',
  lastName: '',
  password: '',
  userType: ''
};

const INITIAL_STATE: AddUserPageState = {
  formData: INITIAL_FORM_DATA,
  errors: {},
  isLoading: false,
  message: '',
  messageType: ''
};

const MAX_LENGTHS = {
  userId: 8,
  firstName: 20,
  lastName: 20,
  password: 8,
  userType: 1
};

export default function AddUserPage() {
  const router = useRouter();
  const [state, setState] = useState<AddUserPageState>(INITIAL_STATE);

  const updateState = useCallback((updates: Partial<AddUserPageState>) => {
    setState(prev => ({ ...prev, ...updates }));
  }, []);

  const setMessage = useCallback((message: string, type: 'success' | 'error' | 'info' = 'info') => {
    updateState({ message, messageType: type });
  }, [updateState]);

  const validateField = useCallback((name: keyof UserFormData, value: string): string => {
    switch (name) {
      case 'userId':
        if (!value || value.trim() === '') return 'User ID can NOT be empty...';
        if (value.length > MAX_LENGTHS.userId) return `User ID cannot exceed ${MAX_LENGTHS.userId} characters`;
        return '';
      
      case 'firstName':
        if (!value || value.trim() === '') return 'First Name can NOT be empty...';
        if (value.length > MAX_LENGTHS.firstName) return `First Name cannot exceed ${MAX_LENGTHS.firstName} characters`;
        return '';
      
      case 'lastName':
        if (!value || value.trim() === '') return 'Last Name can NOT be empty...';
        if (value.length > MAX_LENGTHS.lastName) return `Last Name cannot exceed ${MAX_LENGTHS.lastName} characters`;
        return '';
      
      case 'password':
        if (!value || value.trim() === '') return 'Password can NOT be empty...';
        if (value.length > MAX_LENGTHS.password) return `Password cannot exceed ${MAX_LENGTHS.password} characters`;
        return '';
      
      case 'userType':
        if (!value || value.trim() === '') return 'User Type can NOT be empty...';
        if (!['A', 'R'].includes(value)) return 'User Type must be A (Admin) or R (Regular)';
        return '';
      
      default:
        return '';
    }
  }, []);

  const validateForm = useCallback((): { isValid: boolean; firstErrorField?: keyof UserFormData } => {
    const newErrors: UserFormErrors = {};
    let isValid = true;
    let firstErrorField: keyof UserFormData | undefined;

    // Validate in specific order as per business rules
    const fieldsToValidate: (keyof UserFormData)[] = ['firstName', 'lastName', 'userId', 'password', 'userType'];
    
    for (const fieldName of fieldsToValidate) {
      const value = state.formData[fieldName] as string;
      const error = validateField(fieldName, value);
      
      if (error) {
        newErrors[fieldName] = error;
        if (isValid) {
          firstErrorField = fieldName;
        }
        isValid = false;
      }
    }

    updateState({ errors: newErrors });
    return { isValid, firstErrorField };
  }, [state.formData, validateField, updateState]);

  const handleInputChange = useCallback((field: keyof UserFormData, value: string) => {
    // Apply max length restrictions and formatting
    let processedValue = value;
    
    switch (field) {
      case 'userId':
        processedValue = value.slice(0, MAX_LENGTHS.userId).toUpperCase();
        break;
      case 'firstName':
        processedValue = value.slice(0, MAX_LENGTHS.firstName);
        break;
      case 'lastName':
        processedValue = value.slice(0, MAX_LENGTHS.lastName);
        break;
      case 'password':
        processedValue = value.slice(0, MAX_LENGTHS.password);
        break;
      case 'userType':
        processedValue = value.slice(0, MAX_LENGTHS.userType).toUpperCase();
        break;
    }

    const newFormData = {
      ...state.formData,
      [field]: processedValue
    };

    // Clear field error when user starts typing
    const newErrors = { ...state.errors };
    if (newErrors[field]) {
      delete newErrors[field];
    }

    updateState({
      formData: newFormData,
      errors: newErrors,
      message: '',
      messageType: ''
    });
  }, [state.formData, state.errors, updateState]);

  const handleEnter = useCallback(async () => {
    const validation = validateForm();
    
    if (!validation.isValid) {
      if (validation.firstErrorField) {
        setMessage(state.errors[validation.firstErrorField] || 'Please correct the errors', 'error');
        // Focus on the first error field
        const element = document.getElementById(validation.firstErrorField);
        if (element) {
          element.focus();
        }
      }
      return;
    }

    updateState({ isLoading: true });

    try {
      const createRequest: UserCreateRequest = {
        userId: state.formData.userId.trim(),
        firstName: state.formData.firstName.trim(),
        lastName: state.formData.lastName.trim(),
        password: state.formData.password.trim(),
        userType: state.formData.userType.trim()
      };

      const newUser = await userService.createUser(createRequest);
      
      // Clear form and show success message
      updateState({
        formData: INITIAL_FORM_DATA,
        errors: {},
        isLoading: false
      });
      
      setMessage(`User ${newUser.userId} has been added ...`, 'success');

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to Add User...';
      
      // Handle specific error cases
      if (errorMessage.toLowerCase().includes('duplicate') || 
          errorMessage.toLowerCase().includes('already exist') ||
          errorMessage.toLowerCase().includes('conflict')) {
        updateState({
          errors: { userId: 'User ID already exist...' },
          isLoading: false
        });
        setMessage('User ID already exist...', 'error');
        // Focus on User ID field
        const element = document.getElementById('userId');
        if (element) {
          element.focus();
        }
      } else {
        setMessage('Unable to Add User...', 'error');
        updateState({ isLoading: false });
        // Focus on First Name field for general errors
        const element = document.getElementById('firstName');
        if (element) {
          element.focus();
        }
      }
    }
  }, [state.formData, state.errors, validateForm, updateState, setMessage]);

  const handlePF3Exit = useCallback(() => {
    router.push('/admin');
  }, [router]);

  const handlePF4Clear = useCallback(() => {
    updateState({
      formData: INITIAL_FORM_DATA,
      errors: {},
      message: '',
      messageType: ''
    });
    // Focus on First Name field after clearing
    setTimeout(() => {
      const element = document.getElementById('firstName');
      if (element) {
        element.focus();
      }
    }, 0);
  }, [updateState]);

  const handleKeyDown = useCallback((event: KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        if (!state.isLoading) {
          handleEnter();
        }
        break;
      
      case 'F3':
        event.preventDefault();
        handlePF3Exit();
        break;
      
      case 'F4':
        event.preventDefault();
        handlePF4Clear();
        break;
      
      default:
        // Handle invalid function keys
        if (event.key.startsWith('F') && !['F3', 'F4'].includes(event.key)) {
          event.preventDefault();
          setMessage('INVALID KEY PRESSED', 'error');
        }
        break;
    }
  }, [state.isLoading, handleEnter, handlePF3Exit, handlePF4Clear, setMessage]);

  // Set up keyboard event listeners
  useEffect(() => {
    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [handleKeyDown]);

  // Focus on first name field on mount
  useEffect(() => {
    const element = document.getElementById('firstName');
    if (element) {
      element.focus();
    }
  }, []);

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-2xl mx-auto">
        {/* Header */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold text-gray-900 mb-2">CARDDEMO</h1>
              <h2 className="text-lg text-gray-700">User Add</h2>
            </div>
            <div className="text-right text-sm text-gray-600">
              <div>Program: COUSR01C</div>
              <div>Transaction: CU01</div>
              <div>{new Date().toLocaleDateString()}</div>
              <div>{new Date().toLocaleTimeString()}</div>
            </div>
          </div>
        </div>

        {/* Message Display */}
        {state.message && (
          <div className={`mb-6 p-4 rounded-lg border ${
            state.messageType === 'success' ? 'bg-green-50 border-green-200 text-green-800' :
            state.messageType === 'error' ? 'bg-red-50 border-red-200 text-red-800' :
            'bg-blue-50 border-blue-200 text-blue-800'
          }`}>
            {state.message}
          </div>
        )}

        {/* Form */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
          <form onSubmit={(e) => { e.preventDefault(); handleEnter(); }} className="space-y-6">
            {/* First Name Field */}
            <div>
              <label htmlFor="firstName" className="block text-sm font-medium text-gray-700 mb-2">
                First Name <span className="text-red-500">*</span>
                <span className="text-gray-500 text-xs ml-2">(Max {MAX_LENGTHS.firstName} characters)</span>
              </label>
              <Input
                id="firstName"
                type="text"
                value={state.formData.firstName}
                onChange={(e) => handleInputChange('firstName', e.target.value)}
                placeholder="Enter First Name"
                className={`w-full ${state.errors.firstName ? 'border-red-500 focus:border-red-500' : ''}`}
                disabled={state.isLoading}
                maxLength={MAX_LENGTHS.firstName}
              />
              {state.errors.firstName && (
                <p className="mt-1 text-sm text-red-600">{state.errors.firstName}</p>
              )}
            </div>

            {/* Last Name Field */}
            <div>
              <label htmlFor="lastName" className="block text-sm font-medium text-gray-700 mb-2">
                Last Name <span className="text-red-500">*</span>
                <span className="text-gray-500 text-xs ml-2">(Max {MAX_LENGTHS.lastName} characters)</span>
              </label>
              <Input
                id="lastName"
                type="text"
                value={state.formData.lastName}
                onChange={(e) => handleInputChange('lastName', e.target.value)}
                placeholder="Enter Last Name"
                className={`w-full ${state.errors.lastName ? 'border-red-500 focus:border-red-500' : ''}`}
                disabled={state.isLoading}
                maxLength={MAX_LENGTHS.lastName}
              />
              {state.errors.lastName && (
                <p className="mt-1 text-sm text-red-600">{state.errors.lastName}</p>
              )}
            </div>

            {/* User ID Field */}
            <div>
              <label htmlFor="userId" className="block text-sm font-medium text-gray-700 mb-2">
                User ID <span className="text-red-500">*</span>
                <span className="text-gray-500 text-xs ml-2">(Max {MAX_LENGTHS.userId} characters)</span>
              </label>
              <Input
                id="userId"
                type="text"
                value={state.formData.userId}
                onChange={(e) => handleInputChange('userId', e.target.value)}
                placeholder="Enter User ID"
                className={`w-full ${state.errors.userId ? 'border-red-500 focus:border-red-500' : ''}`}
                disabled={state.isLoading}
                maxLength={MAX_LENGTHS.userId}
              />
              {state.errors.userId && (
                <p className="mt-1 text-sm text-red-600">{state.errors.userId}</p>
              )}
            </div>

            {/* Password Field */}
            <div>
              <label htmlFor="password" className="block text-sm font-medium text-gray-700 mb-2">
                Password <span className="text-red-500">*</span>
                <span className="text-gray-500 text-xs ml-2">(Max {MAX_LENGTHS.password} characters)</span>
              </label>
              <Input
                id="password"
                type="password"
                value={state.formData.password}
                onChange={(e) => handleInputChange('password', e.target.value)}
                placeholder="Enter Password"
                className={`w-full ${state.errors.password ? 'border-red-500 focus:border-red-500' : ''}`}
                disabled={state.isLoading}
                maxLength={MAX_LENGTHS.password}
              />
              {state.errors.password && (
                <p className="mt-1 text-sm text-red-600">{state.errors.password}</p>
              )}
            </div>

            {/* User Type Field */}
            <div>
              <label htmlFor="userType" className="block text-sm font-medium text-gray-700 mb-2">
                User Type <span className="text-red-500">*</span>
              </label>
              <Select
                id="userType"
                value={state.formData.userType}
                onChange={(e) => handleInputChange('userType', e.target.value)}
                options={[
                  { value: '', label: 'Select User Type' },
                  ...USER_TYPE_OPTIONS
                ]}
                className={`w-full ${state.errors.userType ? 'border-red-500 focus:border-red-500' : ''}`}
                disabled={state.isLoading}
              />
              {state.errors.userType && (
                <p className="mt-1 text-sm text-red-600">{state.errors.userType}</p>
              )}
            </div>
          </form>
        </div>

        {/* Action Buttons */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
          <div className="flex flex-wrap gap-4 justify-between">
            <div className="flex gap-3">
              <Button
                onClick={handleEnter}
                disabled={state.isLoading}
                className="bg-green-600 hover:bg-green-700 text-white flex items-center gap-2"
              >
                {state.isLoading ? (
                  <>
                    <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-white"></div>
                    Creating...
                  </>
                ) : (
                  'Enter - Create User'
                )}
              </Button>
              
              <Button
                onClick={handlePF4Clear}
                disabled={state.isLoading}
                variant="outline"
              >
                F4 - Clear
              </Button>
            </div>

            <Button
              onClick={handlePF3Exit}
              disabled={state.isLoading}
              variant="outline"
              className="text-gray-600 hover:text-gray-800"
            >
              F3 - Exit
            </Button>
          </div>
        </div>

        {/* Instructions */}
        <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
          <h4 className="font-semibold text-blue-900 mb-2">Instructions:</h4>
          <ul className="text-sm text-blue-800 space-y-1">
            <li>• Fill in all required fields marked with *</li>
            <li>• Press Enter to create the user</li>
            <li>• Press F3 to exit to Admin Menu</li>
            <li>• Press F4 to clear all fields</li>
            <li>• User Type: A = Admin, R = Regular</li>
          </ul>
        </div>
      </div>
    </div>
  );
}