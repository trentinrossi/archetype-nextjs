'use client';

import { useState, useEffect, useCallback } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import userSecurityService from '@/services/userSecurityService';
import {
  UserSecurity,
  UpdateUserSecurityRequest,
  UpdateUserFormData,
  FormErrors,
  InvalidKeyRequest
} from '@/types/userSecurity';

interface UserUpdateState {
  isLoading: boolean;
  isLookingUp: boolean;
  isSaving: boolean;
  error: string;
  message: string;
  currentDateTime: string;
  userFound: boolean;
  hasChanges: boolean;
  lookupUserId: string;
  originalUser: UserSecurity | null;
  formData: UpdateUserFormData;
  errors: FormErrors;
}

export default function UserUpdatePage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const prefilledUserId = searchParams.get('userId') || '';

  const [state, setState] = useState<UserUpdateState>({
    isLoading: false,
    isLookingUp: false,
    isSaving: false,
    error: '',
    message: '',
    currentDateTime: '',
    userFound: false,
    hasChanges: false,
    lookupUserId: prefilledUserId,
    originalUser: null,
    formData: {
      email: '',
      firstName: '',
      lastName: '',
      roles: [],
      isActive: true,
      mustChangePassword: false
    },
    errors: {}
  });

  // Update current date and time
  const updateDateTime = useCallback((): void => {
    const now = new Date();
    const dateTimeString = now.toLocaleString('en-US', {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
      hour12: false
    });
    setState(prev => ({ ...prev, currentDateTime: dateTimeString }));
  }, []);

  // Initialize page
  useEffect(() => {
    updateDateTime();
    
    // Update time every second
    const interval = setInterval(updateDateTime, 1000);
    
    setState(prev => ({ 
      ...prev, 
      message: 'Enter User ID and press Enter to lookup user for updating.' 
    }));

    // If userId is prefilled, automatically lookup the user
    if (prefilledUserId) {
      handleUserLookup(prefilledUserId);
    }

    return () => clearInterval(interval);
  }, [updateDateTime, prefilledUserId]);

  // Check for changes
  const checkForChanges = useCallback((formData: UpdateUserFormData, originalUser: UserSecurity | null): boolean => {
    if (!originalUser) return false;

    return (
      formData.email !== originalUser.email ||
      formData.firstName !== originalUser.firstName ||
      formData.lastName !== originalUser.lastName ||
      formData.isActive !== originalUser.isActive ||
      formData.mustChangePassword !== originalUser.mustChangePassword ||
      JSON.stringify(formData.roles.sort()) !== JSON.stringify(originalUser.roles.sort())
    );
  }, []);

  // Update hasChanges when form data changes
  useEffect(() => {
    const hasChanges = checkForChanges(state.formData, state.originalUser);
    if (hasChanges !== state.hasChanges) {
      setState(prev => ({ ...prev, hasChanges }));
    }
  }, [state.formData, state.originalUser, state.hasChanges, checkForChanges]);

  // Validate form data
  const validateForm = useCallback((formData: UpdateUserFormData): FormErrors => {
    const errors: FormErrors = {};

    if (!formData.firstName.trim()) {
      errors.firstName = 'First Name is required';
    }

    if (!formData.lastName.trim()) {
      errors.lastName = 'Last Name is required';
    }

    if (!formData.email.trim()) {
      errors.email = 'Email is required';
    } else if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(formData.email)) {
      errors.email = 'Please enter a valid email address';
    }

    if (formData.roles.length === 0) {
      errors.roles = 'At least one role must be selected';
    }

    return errors;
  }, []);

  // Handle user lookup
  const handleUserLookup = useCallback(async (userId?: string): Promise<void> => {
    const userIdToLookup = userId || state.lookupUserId.trim();
    
    if (!userIdToLookup) {
      setState(prev => ({ 
        ...prev, 
        error: 'Please enter a User ID to lookup.' 
      }));
      return;
    }

    setState(prev => ({ 
      ...prev, 
      isLookingUp: true, 
      error: '', 
      message: `Looking up user: ${userIdToLookup}...`,
      userFound: false,
      hasChanges: false
    }));

    try {
      const response = await userSecurityService.getUserById(userIdToLookup);

      if (response.success && response.data?.user) {
        const user = response.data.user;
        
        setState(prev => ({
          ...prev,
          isLookingUp: false,
          userFound: true,
          originalUser: user,
          message: `User found: ${user.firstName} ${user.lastName}`,
          formData: {
            email: user.email,
            firstName: user.firstName,
            lastName: user.lastName,
            roles: [...user.roles],
            isActive: user.isActive,
            mustChangePassword: user.mustChangePassword
          },
          errors: {}
        }));
      } else {
        setState(prev => ({
          ...prev,
          isLookingUp: false,
          userFound: false,
          originalUser: null,
          error: response.error || `User not found: ${userIdToLookup}`,
          formData: {
            email: '',
            firstName: '',
            lastName: '',
            roles: [],
            isActive: true,
            mustChangePassword: false
          }
        }));
      }
    } catch (error) {
      console.error('User lookup error:', error);
      setState(prev => ({
        ...prev,
        isLookingUp: false,
        userFound: false,
        originalUser: null,
        error: error instanceof Error ? error.message : 'Failed to lookup user'
      }));
    }
  }, [state.lookupUserId]);

  // Handle form field changes
  const handleFieldChange = useCallback((field: keyof UpdateUserFormData, value: any): void => {
    setState(prev => ({
      ...prev,
      formData: {
        ...prev.formData,
        [field]: value
      },
      errors: {
        ...prev.errors,
        [field]: undefined
      },
      error: '',
      message: ''
    }));
  }, []);

  // Handle role changes
  const handleRoleChange = useCallback((role: string, checked: boolean): void => {
    setState(prev => {
      const newRoles = checked
        ? [...prev.formData.roles, role]
        : prev.formData.roles.filter(r => r !== role);

      return {
        ...prev,
        formData: {
          ...prev.formData,
          roles: newRoles
        },
        errors: {
          ...prev.errors,
          roles: undefined
        },
        error: '',
        message: ''
      };
    });
  }, []);

  // Handle save (PF5)
  const handleSave = useCallback(async (): Promise<void> => {
    if (!state.originalUser) {
      setState(prev => ({ ...prev, error: 'No user loaded for updating.' }));
      return;
    }

    if (!state.hasChanges) {
      setState(prev => ({ ...prev, message: 'No changes detected. Nothing to save.' }));
      return;
    }

    const errors = validateForm(state.formData);
    if (Object.keys(errors).length > 0) {
      setState(prev => ({ 
        ...prev, 
        errors, 
        error: 'Please correct the validation errors before saving.' 
      }));
      return;
    }

    setState(prev => ({ 
      ...prev, 
      isSaving: true, 
      error: '', 
      message: 'Saving user changes...' 
    }));

    try {
      const updateRequest: UpdateUserSecurityRequest = {
        email: state.formData.email,
        firstName: state.formData.firstName,
        lastName: state.formData.lastName,
        roles: state.formData.roles,
        isActive: state.formData.isActive,
        mustChangePassword: state.formData.mustChangePassword
      };

      const response = await userSecurityService.updateUser(state.originalUser.userId, updateRequest);

      if (response.success) {
        setState(prev => ({
          ...prev,
          isSaving: false,
          hasChanges: false,
          originalUser: response.data || prev.originalUser,
          message: 'User updated successfully.',
          error: ''
        }));
      } else {
        setState(prev => ({
          ...prev,
          isSaving: false,
          error: response.error || 'Failed to update user'
        }));
      }
    } catch (error) {
      console.error('User update error:', error);
      setState(prev => ({
        ...prev,
        isSaving: false,
        error: error instanceof Error ? error.message : 'Failed to update user'
      }));
    }
  }, [state.originalUser, state.hasChanges, state.formData, validateForm]);

  // Handle save and exit (PF3)
  const handleSaveAndExit = useCallback(async (): Promise<void> => {
    if (state.hasChanges) {
      await handleSave();
      // Wait a moment for the save to complete, then navigate
      setTimeout(() => {
        router.push('/admin');
      }, 1000);
    } else {
      router.push('/admin');
    }
  }, [state.hasChanges, handleSave, router]);

  // Handle clear screen (PF4)
  const handleClear = useCallback((): void => {
    setState(prev => ({
      ...prev,
      lookupUserId: '',
      userFound: false,
      hasChanges: false,
      originalUser: null,
      formData: {
        email: '',
        firstName: '',
        lastName: '',
        roles: [],
        isActive: true,
        mustChangePassword: false
      },
      errors: {},
      error: '',
      message: 'Screen cleared. Enter User ID to lookup another user.'
    }));
  }, []);

  // Handle exit (PF12)
  const handleExit = useCallback((): void => {
    if (state.hasChanges) {
      const confirmExit = window.confirm('You have unsaved changes. Are you sure you want to exit without saving?');
      if (!confirmExit) {
        return;
      }
    }
    router.push('/admin');
  }, [state.hasChanges, router]);

  // Handle invalid key press
  const handleInvalidKey = useCallback(async (key: string): Promise<void> => {
    try {
      const invalidKeyRequest: InvalidKeyRequest = {
        attemptedKey: key,
        timestamp: new Date().toISOString()
      };

      const response = await userSecurityService.handleInvalidKey(invalidKeyRequest);
      
      if (response.success && response.data?.message) {
        setState(prev => ({ ...prev, error: response.data!.message }));
      } else {
        setState(prev => ({ 
          ...prev, 
          error: `Invalid key pressed: ${key}. Use Enter to lookup, PF3 (Save & Exit), PF4 (Clear), PF5 (Save), or PF12 (Exit).` 
        }));
      }
    } catch (error) {
      console.error('Invalid key handling error:', error);
      setState(prev => ({ 
        ...prev, 
        error: `Invalid key pressed: ${key}. Use Enter to lookup, PF3 (Save & Exit), PF4 (Clear), PF5 (Save), or PF12 (Exit).` 
      }));
    }
  }, []);

  // Handle keyboard events
  const handleKeyDown = useCallback(async (event: React.KeyboardEvent): Promise<void> => {
    if (state.isLoading || state.isLookingUp || state.isSaving) {
      return;
    }

    // Clear previous messages for function keys
    if (event.key.startsWith('F') || event.key === 'Enter') {
      event.preventDefault();
      setState(prev => ({ ...prev, error: '', message: '' }));
    }

    switch (event.key) {
      case 'Enter':
        if (!state.userFound) {
          await handleUserLookup();
        }
        break;
      
      case 'F3':
        await handleSaveAndExit();
        break;
      
      case 'F4':
        handleClear();
        break;
      
      case 'F5':
        await handleSave();
        break;
      
      case 'F12':
        handleExit();
        break;
      
      case 'F1':
      case 'F2':
      case 'F6':
      case 'F7':
      case 'F8':
      case 'F9':
      case 'F10':
      case 'F11':
      case 'Escape':
        await handleInvalidKey(event.key);
        break;
      
      default:
        // Handle other special key combinations as invalid
        if (event.ctrlKey || event.altKey || event.metaKey) {
          await handleInvalidKey(`${event.ctrlKey ? 'Ctrl+' : ''}${event.altKey ? 'Alt+' : ''}${event.metaKey ? 'Meta+' : ''}${event.key}`);
        }
        break;
    }
  }, [state.isLoading, state.isLookingUp, state.isSaving, state.userFound, handleUserLookup, handleSaveAndExit, handleClear, handleSave, handleExit, handleInvalidKey]);

  const availableRoles = ['ADMIN', 'USER', 'MANAGER', 'VIEWER'];

  return (
    <div 
      className="min-h-screen bg-gray-50 py-8 px-4 sm:px-6 lg:px-8"
      onKeyDown={handleKeyDown}
      tabIndex={-1}
    >
      <div className="max-w-4xl mx-auto">
        {/* Header */}
        <div className="bg-white shadow rounded-lg p-6 mb-8">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-3xl font-bold text-gray-900">User Update Program</h1>
              <p className="mt-1 text-sm text-gray-600">Transaction ID: COUSR02C</p>
            </div>
            <div className="text-right">
              <p className="text-sm text-gray-600">Current Date/Time:</p>
              <p className="text-lg font-mono text-gray-900">{state.currentDateTime}</p>
            </div>
          </div>
        </div>

        {/* Messages */}
        {state.error && (
          <div className="mb-6 rounded-md bg-red-50 p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-red-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <p className="text-sm text-red-800">{state.error}</p>
              </div>
            </div>
          </div>
        )}

        {state.message && !state.error && (
          <div className="mb-6 rounded-md bg-blue-50 p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-blue-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <p className="text-sm text-blue-800">{state.message}</p>
              </div>
            </div>
          </div>
        )}

        {/* User Lookup Section */}
        <div className="bg-white shadow rounded-lg p-6 mb-8">
          <h2 className="text-xl font-semibold text-gray-900 mb-4">User Lookup</h2>
          
          <div className="flex items-end space-x-4">
            <div className="flex-1">
              <label htmlFor="lookupUserId" className="block text-sm font-medium text-gray-700 mb-2">
                User ID
              </label>
              <Input
                id="lookupUserId"
                type="text"
                value={state.lookupUserId}
                onChange={(e) => setState(prev => ({ ...prev, lookupUserId: e.target.value }))}
                placeholder="Enter User ID to lookup"
                disabled={state.isLookingUp || state.userFound}
                className="w-full"
              />
            </div>
            <Button
              onClick={() => handleUserLookup()}
              disabled={state.isLookingUp || !state.lookupUserId.trim()}
              className="px-6 py-2"
            >
              {state.isLookingUp ? 'Looking up...' : 'Lookup (Enter)'}
            </Button>
          </div>
        </div>

        {/* User Details Form */}
        {state.userFound && state.originalUser && (
          <div className="bg-white shadow rounded-lg p-6 mb-8">
            <div className="flex justify-between items-center mb-6">
              <h2 className="text-xl font-semibold text-gray-900">User Details</h2>
              {state.hasChanges && (
                <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-yellow-100 text-yellow-800">
                  Unsaved Changes
                </span>
              )}
            </div>

            <div className="grid gap-6 md:grid-cols-2">
              {/* User ID (Read-only) */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  User ID
                </label>
                <Input
                  type="text"
                  value={state.originalUser.userId}
                  disabled
                  className="bg-gray-50"
                />
              </div>

              {/* Username (Read-only) */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Username
                </label>
                <Input
                  type="text"
                  value={state.originalUser.username}
                  disabled
                  className="bg-gray-50"
                />
              </div>

              {/* First Name */}
              <div>
                <label htmlFor="firstName" className="block text-sm font-medium text-gray-700 mb-2">
                  First Name *
                </label>
                <Input
                  id="firstName"
                  type="text"
                  value={state.formData.firstName}
                  onChange={(e) => handleFieldChange('firstName', e.target.value)}
                  error={state.errors.firstName}
                  disabled={state.isSaving}
                />
                {state.errors.firstName && (
                  <p className="mt-1 text-sm text-red-600">{state.errors.firstName}</p>
                )}
              </div>

              {/* Last Name */}
              <div>
                <label htmlFor="lastName" className="block text-sm font-medium text-gray-700 mb-2">
                  Last Name *
                </label>
                <Input
                  id="lastName"
                  type="text"
                  value={state.formData.lastName}
                  onChange={(e) => handleFieldChange('lastName', e.target.value)}
                  error={state.errors.lastName}
                  disabled={state.isSaving}
                />
                {state.errors.lastName && (
                  <p className="mt-1 text-sm text-red-600">{state.errors.lastName}</p>
                )}
              </div>

              {/* Email */}
              <div className="md:col-span-2">
                <label htmlFor="email" className="block text-sm font-medium text-gray-700 mb-2">
                  Email Address *
                </label>
                <Input
                  id="email"
                  type="email"
                  value={state.formData.email}
                  onChange={(e) => handleFieldChange('email', e.target.value)}
                  error={state.errors.email}
                  disabled={state.isSaving}
                />
                {state.errors.email && (
                  <p className="mt-1 text-sm text-red-600">{state.errors.email}</p>
                )}
              </div>

              {/* Roles */}
              <div className="md:col-span-2">
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  User Roles *
                </label>
                <div className="grid grid-cols-2 gap-4">
                  {availableRoles.map((role) => (
                    <label key={role} className="flex items-center">
                      <input
                        type="checkbox"
                        checked={state.formData.roles.includes(role)}
                        onChange={(e) => handleRoleChange(role, e.target.checked)}
                        disabled={state.isSaving}
                        className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
                      />
                      <span className="ml-2 text-sm text-gray-700">{role}</span>
                    </label>
                  ))}
                </div>
                {state.errors.roles && (
                  <p className="mt-1 text-sm text-red-600">{state.errors.roles}</p>
                )}
              </div>

              {/* Status Flags */}
              <div>
                <label className="flex items-center">
                  <input
                    type="checkbox"
                    checked={state.formData.isActive}
                    onChange={(e) => handleFieldChange('isActive', e.target.checked)}
                    disabled={state.isSaving}
                    className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
                  />
                  <span className="ml-2 text-sm text-gray-700">User is Active</span>
                </label>
              </div>

              <div>
                <label className="flex items-center">
                  <input
                    type="checkbox"
                    checked={state.formData.mustChangePassword}
                    onChange={(e) => handleFieldChange('mustChangePassword', e.target.checked)}
                    disabled={state.isSaving}
                    className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
                  />
                  <span className="ml-2 text-sm text-gray-700">Must Change Password</span>
                </label>
              </div>
            </div>
          </div>
        )}

        {/* Action Buttons */}
        <div className="flex justify-between">
          <div className="flex space-x-4">
            <Button
              onClick={handleSaveAndExit}
              disabled={state.isLoading || state.isLookingUp || state.isSaving || !state.userFound}
              variant="primary"
              className="px-6 py-2"
            >
              {state.isSaving ? 'Saving...' : 'Save & Exit (F3)'}
            </Button>

            <Button
              onClick={handleClear}
              disabled={state.isLoading || state.isLookingUp || state.isSaving}
              variant="secondary"
              className="px-6 py-2"
            >
              Clear (F4)
            </Button>

            <Button
              onClick={handleSave}
              disabled={state.isLoading || state.isLookingUp || state.isSaving || !state.userFound || !state.hasChanges}
              variant="secondary"
              className="px-6 py-2"
            >
              {state.isSaving ? 'Saving...' : 'Save (F5)'}
            </Button>
          </div>

          <Button
            onClick={handleExit}
            disabled={state.isLoading || state.isLookingUp || state.isSaving}
            variant="secondary"
            className="px-6 py-2"
          >
            Exit (F12)
          </Button>
        </div>

        {/* Instructions */}
        <div className="mt-8 bg-gray-50 rounded-lg p-6">
          <h3 className="text-lg font-medium text-gray-900 mb-4">Program Instructions</h3>
          
          <div className="grid gap-6 md:grid-cols-2">
            <div>
              <h4 className="text-sm font-medium text-gray-900 mb-2">User Lookup:</h4>
              <ul className="text-sm text-gray-600 space-y-1">
                <li>• Enter User ID in the lookup field</li>
                <li>• Press <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">Enter</kbd> to lookup user</li>
                <li>• User details will be displayed if found</li>
                <li>• Error message shown if user not found</li>
              </ul>
            </div>
            
            <div>
              <h4 className="text-sm font-medium text-gray-900 mb-2">Update Operations:</h4>
              <ul className="text-sm text-gray-600 space-y-1">
                <li>• Modify any editable field as needed</li>
                <li>• All fields marked with * are required</li>
                <li>• Changes are tracked automatically</li>
                <li>• Validation occurs before saving</li>
              </ul>
            </div>
          </div>

          <div className="mt-4">
            <h4 className="text-sm font-medium text-gray-900 mb-2">Function Keys:</h4>
            <div className="grid grid-cols-2 gap-4 text-sm text-gray-600">
              <div>
                <li>• <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F3</kbd> - Save changes and exit</li>
                <li>• <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F4</kbd> - Clear screen and start over</li>
              </div>
              <div>
                <li>• <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F5</kbd> - Save changes (stay on screen)</li>
                <li>• <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F12</kbd> - Exit without saving</li>
              </div>
            </div>
          </div>

          <div className="mt-4 p-3 bg-yellow-50 rounded-md">
            <p className="text-sm text-yellow-800">
              <strong>Note:</strong> This program allows modification of existing user details. 
              User ID and Username cannot be changed. All required fields must be completed before saving.
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}