'use client';

import React, { useState, useRef, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { userSecurityService } from '@/services/userSecurityService';
import { UserSecurityDTO, UpdateUserSecurityRequest, ValidationError, UserSecurityActionResult } from '@/types/userSecurity';

interface UserUpdateComponentProps {
  userId?: string;
  onUserUpdated?: (userId: string) => void;
  onExit?: () => void;
  className?: string;
}

const UserUpdateComponent: React.FC<UserUpdateComponentProps> = ({
  userId: preselectedUserId,
  onUserUpdated,
  onExit,
  className = ''
}) => {
  const router = useRouter();
  const [lookupUserId, setLookupUserId] = useState<string>(preselectedUserId || '');
  const [currentUser, setCurrentUser] = useState<UserSecurityDTO | null>(null);
  const [originalUser, setOriginalUser] = useState<UserSecurityDTO | null>(null);
  const [formData, setFormData] = useState<UpdateUserSecurityRequest>({
    firstName: '',
    lastName: '',
    userType: 'GENERAL'
  });

  const [errors, setErrors] = useState<Record<string, string>>({});
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [isLookingUp, setIsLookingUp] = useState<boolean>(false);
  const [successMessage, setSuccessMessage] = useState<string>('');
  const [hasModifications, setHasModifications] = useState<boolean>(false);

  const lookupUserIdRef = useRef<HTMLInputElement>(null);
  const firstNameRef = useRef<HTMLInputElement>(null);

  useEffect(() => {
    if (preselectedUserId) {
      handleLookupUser();
    } else if (lookupUserIdRef.current) {
      lookupUserIdRef.current.focus();
    }
  }, [preselectedUserId]);

  useEffect(() => {
    if (currentUser && originalUser) {
      const hasChanges = 
        formData.firstName !== originalUser.firstName ||
        formData.lastName !== originalUser.lastName ||
        formData.userType !== originalUser.userType;
      
      setHasModifications(hasChanges);
    }
  }, [formData, currentUser, originalUser]);

  const clearErrors = () => {
    setErrors({});
    setSuccessMessage('');
  };

  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {};

    // COUSR02C business rules
    if (!formData.firstName?.trim()) {
      newErrors.firstName = 'First Name can NOT be empty...';
    }

    if (!formData.lastName?.trim()) {
      newErrors.lastName = 'Last Name can NOT be empty...';
    }

    if (!formData.userType) {
      newErrors.userType = 'User Type can NOT be empty...';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleLookupUser = async () => {
    const userIdToLookup = preselectedUserId || lookupUserId.trim();
    
    if (!userIdToLookup) {
      setErrors({ lookupUserId: 'User ID can NOT be empty...' });
      return;
    }

    setIsLookingUp(true);
    clearErrors();
    setCurrentUser(null);
    setOriginalUser(null);
    setHasModifications(false);

    try {
      const response = await userSecurityService.getUserById(userIdToLookup);

      if (response.success && response.data) {
        const user = response.data;
        setCurrentUser(user);
        setOriginalUser(user);
        setFormData({
          firstName: user.firstName,
          lastName: user.lastName,
          userType: user.userType
        });

        setSuccessMessage('Press PF5 key to save your updates ...');

        // Focus on first editable field after successful lookup
        setTimeout(() => {
          if (firstNameRef.current) {
            firstNameRef.current.focus();
          }
        }, 100);
      } else {
        setErrors({ general: 'User ID NOT found...' });
        setCurrentUser(null);
        setOriginalUser(null);
        setFormData({
          firstName: '',
          lastName: '',
          userType: 'GENERAL'
        });
      }
    } catch (error) {
      console.error('Error looking up user:', error);
      setErrors({ general: 'Unable to lookup User...' });
    } finally {
      setIsLookingUp(false);
    }
  };

  const handleInputChange = (field: keyof UpdateUserSecurityRequest, value: string) => {
    setFormData(prev => ({
      ...prev,
      [field]: value
    }));

    // Clear field-specific error when user starts typing
    if (errors[field]) {
      setErrors(prev => ({
        ...prev,
        [field]: ''
      }));
    }

    // Clear success message when form is modified
    if (successMessage) {
      setSuccessMessage('');
    }
  };

  const handleUpdate = async () => {
    if (!currentUser) {
      setErrors({ general: 'No user loaded for update' });
      return;
    }

    if (!hasModifications) {
      setErrors({ general: 'Please modify to update ...' });
      return;
    }

    if (!validateForm()) {
      return;
    }

    setIsLoading(true);
    clearErrors();

    try {
      const result: UserSecurityActionResult = await userSecurityService.updateUserWithResult(
        currentUser.userId,
        formData
      );

      if (result.success && result.data) {
        setSuccessMessage(`User ${currentUser.userId} has been updated ...`);
        setCurrentUser(result.data);
        setOriginalUser(result.data);
        setHasModifications(false);

        // Notify parent component
        if (onUserUpdated) {
          onUserUpdated(result.data.userId);
        }
      } else {
        if (result.message.includes('NOT found')) {
          setErrors({ general: 'User ID NOT found...' });
        } else {
          setErrors({ general: result.message || 'Unable to Update User...' });
        }
      }
    } catch (error) {
      console.error('Error updating user:', error);
      setErrors({ general: 'Unable to Update User...' });
    } finally {
      setIsLoading(false);
    }
  };

  const handleSaveAndExit = async () => {
    if (!currentUser) {
      handleExit();
      return;
    }

    if (!hasModifications) {
      handleExit();
      return;
    }

    await handleUpdate();
    if (!errors.general) {
      handleExit();
    }
  };

  const handleClear = () => {
    setLookupUserId('');
    setCurrentUser(null);
    setOriginalUser(null);
    setFormData({
      firstName: '',
      lastName: '',
      userType: 'GENERAL'
    });
    clearErrors();
    setHasModifications(false);

    if (lookupUserIdRef.current) {
      lookupUserIdRef.current.focus();
    }
  };

  const handleExit = () => {
    if (onExit) {
      onExit();
    } else {
      router.push('/admin/dashboard');
    }
  };

  const handleKeyDown = (event: React.KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        if (!currentUser) {
          handleLookupUser();
        } else {
          handleUpdate();
        }
        break;

      case 'F3':
        event.preventDefault();
        if (!isLoading) {
          handleSaveAndExit();
        }
        break;

      case 'F4':
        event.preventDefault();
        if (!isLoading) {
          handleClear();
        }
        break;

      case 'F5':
        event.preventDefault();
        if (!isLoading) {
          handleUpdate();
        }
        break;

      case 'F12':
        event.preventDefault();
        if (!isLoading) {
          handleExit();
        }
        break;

      default:
        // Allow normal key processing
        break;
    }
  };

  return (
    <div 
      className={`min-h-screen bg-gray-50 p-6 ${className}`}
      onKeyDown={handleKeyDown}
      tabIndex={-1}
    >
      <div className="max-w-4xl mx-auto">
        {/* Header */}
        <div className="mb-6">
          <h1 className="text-2xl font-bold text-gray-900 mb-2">
            CardDemo
          </h1>
          <h2 className="text-xl text-gray-700 mb-2">
            User Update
          </h2>
          <div className="flex justify-between text-sm text-gray-600">
            <span>COUSR02C - CU02</span>
            <span>{new Date().toLocaleDateString()} {new Date().toLocaleTimeString()}</span>
          </div>
        </div>

        {/* Success Message */}
        {successMessage && (
          <div className="bg-green-50 border border-green-200 rounded-md p-4 mb-6">
            <p className="text-sm text-green-800 font-medium">{successMessage}</p>
          </div>
        )}

        {/* General Error */}
        {errors.general && (
          <div className="bg-red-50 border border-red-200 rounded-md p-4 mb-6">
            <p className="text-sm text-red-800">{errors.general}</p>
          </div>
        )}

        {/* User Lookup Section */}
        {!preselectedUserId && (
          <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
            <div className="flex gap-4 items-end">
              <div className="flex-1">
                <label htmlFor="lookupUserId" className="block text-sm font-medium text-gray-700 mb-1">
                  User ID:
                </label>
                <input
                  ref={lookupUserIdRef}
                  id="lookupUserId"
                  type="text"
                  value={lookupUserId}
                  onChange={(e) => {
                    setLookupUserId(e.target.value.toUpperCase());
                    if (errors.lookupUserId) {
                      setErrors(prev => ({ ...prev, lookupUserId: '' }));
                    }
                  }}
                  placeholder="Enter User ID to lookup"
                  className={`w-full px-3 py-2 border rounded-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 ${
                    errors.lookupUserId ? 'border-red-500' : 'border-gray-300'
                  }`}
                  disabled={isLookingUp}
                  maxLength={8}
                />
                {errors.lookupUserId && (
                  <p className="mt-1 text-sm text-red-600">{errors.lookupUserId}</p>
                )}
              </div>
            </div>
          </div>
        )}

        {/* User Details Form - Only show if user is loaded */}
        {currentUser && (
          <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              {/* User ID (Read-only) */}
              <div>
                <label htmlFor="displayUserId" className="block text-sm font-medium text-gray-700 mb-1">
                  User ID:
                </label>
                <input
                  id="displayUserId"
                  type="text"
                  value={currentUser.userId}
                  readOnly
                  className="w-full px-3 py-2 border border-gray-300 rounded-md bg-gray-50"
                />
              </div>

              {/* First Name */}
              <div>
                <label htmlFor="firstName" className="block text-sm font-medium text-gray-700 mb-1">
                  First Name:
                </label>
                <input
                  ref={firstNameRef}
                  id="firstName"
                  type="text"
                  value={formData.firstName || ''}
                  onChange={(e) => handleInputChange('firstName', e.target.value)}
                  placeholder="Enter first name"
                  className={`w-full px-3 py-2 border rounded-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 ${
                    errors.firstName ? 'border-red-500' : 'border-gray-300'
                  }`}
                  disabled={isLoading}
                  maxLength={20}
                />
                {errors.firstName && (
                  <p className="mt-1 text-sm text-red-600">{errors.firstName}</p>
                )}
              </div>

              {/* Last Name */}
              <div>
                <label htmlFor="lastName" className="block text-sm font-medium text-gray-700 mb-1">
                  Last Name:
                </label>
                <input
                  id="lastName"
                  type="text"
                  value={formData.lastName || ''}
                  onChange={(e) => handleInputChange('lastName', e.target.value)}
                  placeholder="Enter last name"
                  className={`w-full px-3 py-2 border rounded-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 ${
                    errors.lastName ? 'border-red-500' : 'border-gray-300'
                  }`}
                  disabled={isLoading}
                  maxLength={20}
                />
                {errors.lastName && (
                  <p className="mt-1 text-sm text-red-600">{errors.lastName}</p>
                )}
              </div>

              {/* User Type */}
              <div>
                <label htmlFor="userType" className="block text-sm font-medium text-gray-700 mb-1">
                  User Type:
                </label>
                <select
                  id="userType"
                  value={formData.userType || 'GENERAL'}
                  onChange={(e) => handleInputChange('userType', e.target.value as 'ADMIN' | 'GENERAL')}
                  className={`w-full px-3 py-2 border rounded-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 ${
                    errors.userType ? 'border-red-500' : 'border-gray-300'
                  }`}
                  disabled={isLoading}
                >
                  <option value="GENERAL">General User</option>
                  <option value="ADMIN">Administrator</option>
                </select>
                {errors.userType && (
                  <p className="mt-1 text-sm text-red-600">{errors.userType}</p>
                )}
              </div>
            </div>
          </div>
        )}

        {/* Action Buttons */}
        <div className="flex flex-wrap gap-4 mb-6">
          {!currentUser ? (
            <>
              <button
                onClick={handleLookupUser}
                disabled={isLookingUp || !lookupUserId.trim()}
                className="flex-1 sm:flex-none min-w-[120px] bg-indigo-600 text-white px-4 py-2 rounded-md hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                {isLookingUp ? 'Looking up...' : 'Lookup (Enter)'}
              </button>
              
              <button
                onClick={handleClear}
                disabled={isLookingUp}
                className="flex-1 sm:flex-none min-w-[120px] bg-gray-600 text-white px-4 py-2 rounded-md hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-gray-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                Clear (F4)
              </button>
              
              <button
                onClick={handleExit}
                disabled={isLookingUp}
                className="flex-1 sm:flex-none min-w-[120px] bg-gray-600 text-white px-4 py-2 rounded-md hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-gray-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                Exit (F3)
              </button>
            </>
          ) : (
            <>
              <button
                onClick={handleUpdate}
                disabled={isLoading || !hasModifications}
                className="flex-1 sm:flex-none min-w-[120px] bg-indigo-600 text-white px-4 py-2 rounded-md hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                {isLoading ? 'Updating...' : 'Update (F5)'}
              </button>
              
              <button
                onClick={handleSaveAndExit}
                disabled={isLoading}
                className="flex-1 sm:flex-none min-w-[140px] bg-indigo-600 text-white px-4 py-2 rounded-md hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                {isLoading ? 'Updating...' : 'Update & Exit (F3)'}
              </button>
              
              <button
                onClick={handleClear}
                disabled={isLoading}
                className="flex-1 sm:flex-none min-w-[120px] bg-gray-600 text-white px-4 py-2 rounded-md hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-gray-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                Clear (F4)
              </button>
              
              <button
                onClick={handleExit}
                disabled={isLoading}
                className="flex-1 sm:flex-none min-w-[120px] bg-gray-600 text-white px-4 py-2 rounded-md hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-gray-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                Cancel (F12)
              </button>
            </>
          )}
        </div>

        {/* Function Key Instructions */}
        <div className="bg-gray-50 border border-gray-200 rounded-md p-4">
          <h3 className="text-sm font-medium text-gray-900 mb-2">Instructions</h3>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-2 text-xs text-gray-600">
            <div><span className="font-medium">Enter:</span> Lookup user / Update user</div>
            <div><span className="font-medium">F3:</span> Update and exit</div>
            <div><span className="font-medium">F4:</span> Clear all fields</div>
            <div><span className="font-medium">F5:</span> Update user</div>
            <div><span className="font-medium">F12:</span> Cancel/Exit</div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default UserUpdateComponent;