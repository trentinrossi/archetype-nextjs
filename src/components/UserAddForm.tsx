'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/Select';
import { userSecurityService } from '@/services/userSecurityService';
import { 
  CreateUserSecurityRequest, 
  UserSecurityDTO,
  InvalidKeyResponseDTO 
} from '@/types/user-security';

interface UserAddFormProps {
  programName?: string;
  transactionId?: string;
  systemId?: string;
  workstationId?: string;
}

export default function UserAddForm({
  programName = 'COUSR01C',
  transactionId = 'USERADD',
  systemId = 'SYSTEM',
  workstationId = 'WS001'
}: UserAddFormProps) {
  const router = useRouter();
  
  // Form state
  const [formData, setFormData] = useState<CreateUserSecurityRequest>({
    userId: '',
    userName: '',
    password: '',
    confirmPassword: '',
    userType: 'USER',
    userStatus: 'ACTIVE',
    securityLevel: 1,
    userGroup: '',
    department: '',
    email: '',
    phoneNumber: '',
    profileData: {
      firstName: '',
      lastName: '',
      displayName: '',
      title: ''
    }
  });
  
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string>('');
  const [success, setSuccess] = useState<string>('');
  const [currentDateTime, setCurrentDateTime] = useState<string>('');
  const [focusedField, setFocusedField] = useState<string>('firstName');

  // Update current date and time
  useEffect(() => {
    const updateDateTime = () => {
      const now = new Date();
      const dateStr = now.toLocaleDateString('en-US', {
        year: 'numeric',
        month: '2-digit',
        day: '2-digit'
      });
      const timeStr = now.toLocaleTimeString('en-US', {
        hour12: false,
        hour: '2-digit',
        minute: '2-digit',
        second: '2-digit'
      });
      setCurrentDateTime(`${dateStr} ${timeStr}`);
    };

    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    
    return () => clearInterval(interval);
  }, []);

  // Handle form submission
  const handleSubmit = useCallback(async () => {
    setError('');
    setSuccess('');

    // Validate required fields
    if (!formData.profileData?.firstName?.trim()) {
      setError('First Name can NOT be empty...');
      setFocusedField('firstName');
      return;
    }

    if (!formData.profileData?.lastName?.trim()) {
      setError('Last Name can NOT be empty...');
      setFocusedField('lastName');
      return;
    }

    if (!formData.userId.trim()) {
      setError('User ID can NOT be empty...');
      setFocusedField('userId');
      return;
    }

    if (!formData.password.trim()) {
      setError('Password can NOT be empty...');
      setFocusedField('password');
      return;
    }

    if (!formData.userType) {
      setError('User Type can NOT be empty...');
      setFocusedField('userType');
      return;
    }

    if (formData.password !== formData.confirmPassword) {
      setError('Password confirmation does not match...');
      setFocusedField('confirmPassword');
      return;
    }

    setLoading(true);

    try {
      // Prepare user data
      const userData: CreateUserSecurityRequest = {
        ...formData,
        userId: formData.userId.toUpperCase(),
        userName: `${formData.profileData?.firstName} ${formData.profileData?.lastName}`.trim(),
        profileData: {
          ...formData.profileData,
          displayName: `${formData.profileData?.firstName} ${formData.profileData?.lastName}`.trim()
        }
      };

      const newUser: UserSecurityDTO = await userSecurityService.createUserSecurity(userData);

      // Success
      setSuccess(`User ${newUser.userId} has been added...`);
      
      // Clear form after successful creation
      setTimeout(() => {
        handleClearForm();
        setSuccess('');
      }, 3000);

    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Unable to Add User...';
      
      if (errorMessage.includes('already exists') || errorMessage.includes('Conflict')) {
        setError('User ID already exist...');
        setFocusedField('userId');
      } else {
        setError(errorMessage);
        setFocusedField('firstName');
      }
    } finally {
      setLoading(false);
    }
  }, [formData]);

  // Handle clear form (PF4)
  const handleClearForm = useCallback(() => {
    setFormData({
      userId: '',
      userName: '',
      password: '',
      confirmPassword: '',
      userType: 'USER',
      userStatus: 'ACTIVE',
      securityLevel: 1,
      userGroup: '',
      department: '',
      email: '',
      phoneNumber: '',
      profileData: {
        firstName: '',
        lastName: '',
        displayName: '',
        title: ''
      }
    });
    setError('');
    setSuccess('');
    setFocusedField('firstName');
  }, []);

  // Handle exit (PF3)
  const handleExit = useCallback(() => {
    router.push('/dashboard');
  }, [router]);

  // Handle invalid key
  const handleInvalidKey = useCallback(async (key: string) => {
    try {
      const currentUserId = userSecurityService.getStoredUserId() || 'GUEST';
      const invalidKeyRequest = {
        userId: currentUserId,
        invalidKey: key,
        reason: `Invalid function key pressed: ${key}`
      };

      const response: InvalidKeyResponseDTO = await userSecurityService.handleInvalidKey(invalidKeyRequest);
      
      if (response.acknowledged) {
        setError(`Invalid key: ${key}. ${response.message}`);
      }
    } catch (err) {
      setError(`Invalid key: ${key}`);
    }
  }, []);

  // Handle keyboard events
  const handleKeyDown = useCallback((event: React.KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        if (!loading) {
          handleSubmit();
        }
        break;
      case 'F3':
        event.preventDefault();
        if (!loading) {
          handleExit();
        }
        break;
      case 'F4':
        event.preventDefault();
        if (!loading) {
          handleClearForm();
        }
        break;
      case 'F1':
      case 'F2':
      case 'F5':
      case 'F6':
      case 'F7':
      case 'F8':
      case 'F9':
      case 'F10':
      case 'F11':
      case 'F12':
        event.preventDefault();
        handleInvalidKey(event.key);
        break;
      default:
        break;
    }
  }, [loading, handleSubmit, handleExit, handleClearForm, handleInvalidKey]);

  // Handle input changes
  const handleInputChange = (field: string, value: string) => {
    if (field.startsWith('profileData.')) {
      const profileField = field.replace('profileData.', '');
      setFormData(prev => ({
        ...prev,
        profileData: {
          ...prev.profileData,
          [profileField]: value
        }
      }));
    } else {
      setFormData(prev => ({
        ...prev,
        [field]: field === 'userId' ? value.toUpperCase() : value
      }));
    }
    setError('');
    setSuccess('');
  };

  return (
    <div 
      className="min-h-screen bg-black text-green-400 font-mono p-4"
      onKeyDown={handleKeyDown}
      tabIndex={0}
    >
      {/* Screen Header */}
      <div className="border border-green-400 mb-4 p-2">
        <div className="flex justify-between items-center">
          <div className="flex space-x-8">
            <span>Program: {programName}</span>
            <span>Transaction: {transactionId}</span>
          </div>
          <div>
            <span>{currentDateTime}</span>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="max-w-4xl mx-auto">
        {/* Title */}
        <div className="text-center mb-8">
          <h1 className="text-2xl mb-2">ADD NEW USER</h1>
          <div className="text-sm text-green-300">Enter user information to create a new account</div>
        </div>

        {/* User Form */}
        <div className="border border-green-400 p-6 mb-4">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {/* First Name */}
            <div className="flex items-center">
              <label className="w-32 text-right mr-4">First Name:</label>
              <div className="flex-1">
                <Input
                  type="text"
                  value={formData.profileData?.firstName || ''}
                  onChange={(e) => handleInputChange('profileData.firstName', e.target.value)}
                  className="bg-black border-green-400 text-green-400 focus:border-green-300 focus:ring-green-300 font-mono"
                  maxLength={20}
                  placeholder="Enter First Name"
                  disabled={loading}
                  autoFocus={focusedField === 'firstName'}
                />
              </div>
            </div>

            {/* Last Name */}
            <div className="flex items-center">
              <label className="w-32 text-right mr-4">Last Name:</label>
              <div className="flex-1">
                <Input
                  type="text"
                  value={formData.profileData?.lastName || ''}
                  onChange={(e) => handleInputChange('profileData.lastName', e.target.value)}
                  className="bg-black border-green-400 text-green-400 focus:border-green-300 focus:ring-green-300 font-mono"
                  maxLength={20}
                  placeholder="Enter Last Name"
                  disabled={loading}
                  autoFocus={focusedField === 'lastName'}
                />
              </div>
            </div>

            {/* User ID */}
            <div className="flex items-center">
              <label className="w-32 text-right mr-4">User ID:</label>
              <div className="flex-1">
                <Input
                  type="text"
                  value={formData.userId}
                  onChange={(e) => handleInputChange('userId', e.target.value)}
                  className="bg-black border-green-400 text-green-400 focus:border-green-300 focus:ring-green-300 font-mono uppercase"
                  maxLength={8}
                  placeholder="Enter User ID"
                  disabled={loading}
                  autoFocus={focusedField === 'userId'}
                />
              </div>
            </div>

            {/* Password */}
            <div className="flex items-center">
              <label className="w-32 text-right mr-4">Password:</label>
              <div className="flex-1">
                <Input
                  type="password"
                  value={formData.password}
                  onChange={(e) => handleInputChange('password', e.target.value)}
                  className="bg-black border-green-400 text-green-400 focus:border-green-300 focus:ring-green-300 font-mono"
                  maxLength={20}
                  placeholder="Enter Password"
                  disabled={loading}
                  autoFocus={focusedField === 'password'}
                />
              </div>
            </div>

            {/* Confirm Password */}
            <div className="flex items-center">
              <label className="w-32 text-right mr-4">Confirm Password:</label>
              <div className="flex-1">
                <Input
                  type="password"
                  value={formData.confirmPassword}
                  onChange={(e) => handleInputChange('confirmPassword', e.target.value)}
                  className="bg-black border-green-400 text-green-400 focus:border-green-300 focus:ring-green-300 font-mono"
                  maxLength={20}
                  placeholder="Confirm Password"
                  disabled={loading}
                  autoFocus={focusedField === 'confirmPassword'}
                />
              </div>
            </div>

            {/* User Type */}
            <div className="flex items-center">
              <label className="w-32 text-right mr-4">User Type:</label>
              <div className="flex-1">
                <Select
                  value={formData.userType}
                  onValueChange={(value) => handleInputChange('userType', value)}
                  disabled={loading}
                >
                  <SelectTrigger className="bg-black border-green-400 text-green-400 focus:border-green-300 focus:ring-green-300">
                    <SelectValue placeholder="Select User Type" />
                  </SelectTrigger>
                  <SelectContent className="bg-black border-green-400">
                    <SelectItem value="ADMIN" className="text-green-400 hover:bg-green-900/20">ADMIN - Administrator</SelectItem>
                    <SelectItem value="USER" className="text-green-400 hover:bg-green-900/20">USER - General User</SelectItem>
                    <SelectItem value="GUEST" className="text-green-400 hover:bg-green-900/20">GUEST - Guest User</SelectItem>
                  </SelectContent>
                </Select>
              </div>
            </div>

            {/* Department */}
            <div className="flex items-center">
              <label className="w-32 text-right mr-4">Department:</label>
              <div className="flex-1">
                <Input
                  type="text"
                  value={formData.department || ''}
                  onChange={(e) => handleInputChange('department', e.target.value)}
                  className="bg-black border-green-400 text-green-400 focus:border-green-300 focus:ring-green-300 font-mono"
                  maxLength={30}
                  placeholder="Enter Department (Optional)"
                  disabled={loading}
                />
              </div>
            </div>

            {/* Email */}
            <div className="flex items-center">
              <label className="w-32 text-right mr-4">Email:</label>
              <div className="flex-1">
                <Input
                  type="email"
                  value={formData.email || ''}
                  onChange={(e) => handleInputChange('email', e.target.value)}
                  className="bg-black border-green-400 text-green-400 focus:border-green-300 focus:ring-green-300 font-mono"
                  maxLength={50}
                  placeholder="Enter Email (Optional)"
                  disabled={loading}
                />
              </div>
            </div>
          </div>
        </div>

        {/* Success Message */}
        {success && (
          <div className="border border-green-400 bg-green-900/20 text-green-400 p-3 mb-4">
            <div className="font-bold">SUCCESS:</div>
            <div>{success}</div>
          </div>
        )}

        {/* Error Message */}
        {error && (
          <div className="border border-red-400 bg-red-900/20 text-red-400 p-3 mb-4">
            <div className="font-bold">ERROR:</div>
            <div>{error}</div>
          </div>
        )}

        {/* Loading Message */}
        {loading && (
          <div className="border border-yellow-400 bg-yellow-900/20 text-yellow-400 p-3 mb-4">
            <div>Creating user... Please wait</div>
          </div>
        )}

        {/* Function Keys */}
        <div className="border border-green-400 p-4 mb-4">
          <div className="text-sm">
            <div className="mb-2 font-bold">Function Keys:</div>
            <div className="grid grid-cols-2 gap-2">
              <div>ENTER = Add User</div>
              <div>F3 = Exit</div>
              <div>F4 = Clear Screen</div>
            </div>
          </div>
        </div>

        {/* Action Buttons */}
        <div className="flex justify-center space-x-4">
          <Button
            onClick={handleSubmit}
            disabled={loading}
            className="bg-green-700 hover:bg-green-600 text-white border-green-400 font-mono"
          >
            {loading ? 'Adding User...' : 'Add User (ENTER)'}
          </Button>
          
          <Button
            onClick={handleClearForm}
            disabled={loading}
            variant="outline"
            className="border-green-400 text-green-400 hover:bg-green-900/20 font-mono"
          >
            Clear (F4)
          </Button>
          
          <Button
            onClick={handleExit}
            disabled={loading}
            variant="outline"
            className="border-green-400 text-green-400 hover:bg-green-900/20 font-mono"
          >
            Exit (F3)
          </Button>
        </div>

        {/* System Information */}
        <div className="mt-8 text-center text-xs text-green-300">
          <div>System ID: {systemId} | Workstation: {workstationId}</div>
          <div className="mt-2">© 2025 User Security Management System</div>
        </div>
      </div>
    </div>
  );
}